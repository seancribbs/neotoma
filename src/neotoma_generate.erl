%% @doc Implements code-generation using syntax_tools.
-module(neotoma_generate).
-include("neotoma.hrl").
-include_lib("merl/include/merl.hrl").

-compile(export_all).
-import(erl_syntax, [
                     abstract/1,
                     %% add_ann/2,
                     %% application/2,
                     %% application/3,
                     atom/1,
                     %% attribute/1,
                     attribute/2,
                     %% binary/1,
                     %% binary_field/2,
                     block_expr/1,
                     block_expr_body/1,
                     %% case_expr/2,
                     clause/3,
                     cons/2,
                     form_list/1,
                     %% fun_expr/1,
                     function/2,
                     %% list/2,
                     %% match_expr/2,
                     tuple/1,
                     type/1,
                     %% underscore/0,
                     variable/1
                    ]).
-import(erl_recomment, [recomment_forms/2]).

%% -type continuation() :: fun((Result::erl_syntax:syntaxTree(),
%%                              Input::erl_syntax:syntaxTree(),
%%                              Reason::string()) ->
%%                                    [erl_syntax:syntaxTree()]).

%% -type failfun() :: fun((InputVar::erl_syntax:syntaxTree(),
%%                         Reason::string()) -> [erl_syntax:syntaxTree()]).

%% -type successfun() :: fun((CaptureVariable::erl_syntax:syntaxTree(),
%%                            RemainderVariable::erl_syntax:syntaxTree()) ->
%%                                  [erl_syntax:syntaxTree()]).

%% TODO: Fix spec to only allow proper metagrammar syntax nodes
%% TODO: Handle advancement of the index into the input
%% -spec generate(tuple(), InputVar::erl_syntax:syntaxTree(),
%%                Success::continuation(), Fail::continuation()) ->
%%                       erl_syntax:syntaxTree().

generate(#grammar{declarations=Decls, code=Code, filename=FN}, InputName, Success, Fail) ->
    CodeBlock = case Code of
                    #code{index={{line,Line},_}} ->
                        [
                         attribute(atom(file), [abstract(FN), abstract(Line)]),
                         recomment_forms(form_list(Code#code.parsed), Code#code.comments)
                        ];
                    undefined ->
                        []
                end,
    form_list(CodeBlock ++ [generate(D, InputName, Success, Fail) || D <- Decls]);

generate(#declaration{name=Name, expr=E}, InputName, Success, Fail) ->
    %% TODO: Handle #code{}
    %% TODO: memoization
    %% TODO: virtual-inlining
    %% TODO: add comments with rule declaration
    %% TODO: add type-specs (via add_ann?)
    %% TODO: add -file annotations?
    ExprCode = generate(E, InputName, Success, Fail),
    function(abstract(Name), [?Q("(_@InputName) -> _@ExprCode")]);

generate(#choice{alts=[Alt]}, InputName, Success, Fail0) ->
    %% When we have only one alternate left, we should generate its
    %% code with the original Fail function.
    generate(Alt, InputName, Success, Fail0);

generate(#choice{alts=[A|As]}=C, InputName, Success, Fail0) ->
    %% We want to generate a structure that recursively tries each
    %% expression, terminating on the first that succeeds, or failing
    %% if they all fail.
    %%
    %% This means that the Success function is the same at every step,
    %% but the Fail function generates the next alternate in the
    %% chain.
    %%
    %% Unlike the Haskell version in the paper, we can't have "named
    %% expressions" that can be referred to from any related
    %% sub-expression (boo no laziness!). However, we *can* generate
    %% functions that can be called from within the nested structure.
    %%
    %% A / B ==>
    %%
    %% Choice1 = fun(InputVar0) -> {{Generate B, InputVar0, Success, Fail0}} end,
    %% {{Generate A, InputVar, Success, FailByCalling(Choice1)}}
    %%
    %% A / B / C ==>
    %%
    %% begin
    %% Choice1 = fun(InputVar0) ->
    %%       begin
    %%         Choice2 = fun(InputVar1) -> {{Generate C, InputVar1, Success, Fail0}} end,
    %%         begin
    %%           {{Generate B, InputVar0, Success, FailByCalling(Choice2)}}
    %%         end
    %%       end
    %% end,
    %% {{Generate A, InputVar, Success, FailByCalling(Choice1)}}
    %% end
    %%
    %% This is obviously ugly and could be potentially flattened out
    %% with some clever finagling.
    AltName = variable(new_name("Choice")),
    NewInputName = variable(new_name("Input")),
    Fail = fun(IV, _Reason) ->
                   %% TODO: Deal with the failure reason!
                   ?Q("_@AltName(_@IV)")
           end,
    FirstAlt = generate(A, InputName, Success, Fail),

    AltContents = generate(C#choice{alts=As}, NewInputName, Success, Fail0),

    %% Unwrap nested block expressions. This way we don't introduce
    %% them needlessly when a function clause suffices.
    FunContents = case type(AltContents) of
                      block_expr -> block_expr_body(AltContents);
                      _ -> [AltContents]
                  end,

    %% Now set up the expression to call the next alternate on
    %% failure.
    ?Q(["begin ",
        "    _@AltName = fun(_@NewInputName) -> _@FunContents end,",
        "    _@FirstAlt ",
        "end"]);%% ,

generate(#sequence{exprs=[E|Es]}=S, InputName, Success0, Fail) ->
    %% We want to generate a structure that recursively tries each
    %% expression, terminating on the first that fails, or succeeding
    %% if they all succeed, and consing together their results.
    %%
    %% This means, that the Fail function is basically the same at
    %% every step, but the Success function generates the next step in
    %% the chain.
    Success = fun(ResultVar, InputVar) ->
                      %% We need to cons ResultVar onto the
                      %% result of the success chain! brane asplode
                      Conser = fun(NestedResult, IV2) ->
                                       Success0(cons(ResultVar, NestedResult), IV2)
                               end,
                      [generate(S#sequence{exprs=Es}, InputVar, Conser, Fail)]
              end,
    generate(E, InputName, Success, Fail);

generate(#sequence{exprs=[]}, InputName, Success, _Fail) ->
    %% TODO: Special-case epsilon as last entry?
    %%
    %% We have reached the end of the sequence on the success path, so
    %% we simply call Success with an empty list (the tail of the
    %% result captures) and the input name. This should flatten out
    %% all of the results into a single list.
    %%
    %% The below is a hack because the 'Success' callback must always
    %% return a list of expressions suitable for assigning to the body
    %% of a clause, but generate/4 returns a single syntaxTree().
    case Success(abstract([]), InputName) of
        %% If the result is just one expression, simply return it
        %% bare.
        [E] -> E;
        %% If it is a list of expressions, wrap it in a block.
        Exprs -> block_expr(Exprs)
    end;

generate(#label{expr=E, label=L}, InputName, Success0, Fail) ->
    %% When a label is defined and there's no modifier (or we've
    %% already applied it), we wrap the capture in a tuple with the
    %% label.
    Success = fun(Capture, Rest) ->
                      Success0(tuple([atom(L), Capture]), Rest)
              end,
    generate(E, InputName, Success, Fail);

generate(#optional{expr=E}, InputName, Success, _Fail0) ->
    %% If the expression is optional, failure is also success!
    Fail = fun(InputVar, _Reason) ->
                   Success(abstract([]), InputVar)
           end,
    generate(E, InputName, Success, Fail);

generate(#assert{expr=E}, InputName, Success0, Fail) ->
    %% Zero-width positive lookahead. Failed match still fails, but a
    %% successful match does not advance the input.
    Success = fun(_Capture, _Rest) ->
                      Success0(abstract([]), InputName)
              end,
    generate(E, InputName, Success, Fail);

generate(#deny{expr=E}, InputName, Success0, Fail0) ->
    %% Zero-width negative lookahead. Successful match fails, failed
    %% match succeeds but consumes nothing.
    Success = fun(_Capture, _Rest) ->
                      %% TODO: better error reason here
                      Fail0(InputName, deny)
              end,
    Fail = fun(InputVar, _Reason) ->
                   Success0(abstract([]), InputVar)
           end,
    generate(E, InputName, Success, Fail);


generate(#nonterminal{name=NT0}, InputName, Success, Fail) ->
    %% Template:
    %%
    %% case {{NT}}(???) of
    %%   {fail, Reason0} -> {{Fail}};
    %%   {ok, Result0, Rest0} -> {{Success(Result0, Rest0)}}
    %% end
    NT = abstract(NT0),
    ResultName = variable(new_name("Result")),
    RestName = variable(new_name("Input")),
    ReasonName = variable(new_name("Reason")),
    FailBranch = Fail(InputName, ReasonName),
    SuccessBranch = Success(ResultName, RestName),
    ?Q(["case _@NT(_@InputName) of ",
        "  {fail, _@ReasonName} -> _@FailBranch; ",
        "  {ok, _@ResultName, _@RestName} -> _@SuccessBranch "
        "end"]);

generate(#charclass{charclass=C, index=I}, InputName, Success, Fail) ->
    %% TODO: For now, treating character class as a regexp. In the
    %% future, this can be exploded into a more efficient case
    %% statement.
    generate(#regexp{regexp= <<$[,C/binary,$]>>, index=I}, InputName, Success, Fail);

generate(#regexp{regexp=R}, InputName, Success, Fail) ->
    %% Template:
    %%
    %% case re:run(Input0, {{R}}) of
    %%   {match, [{0, Length0}|_]} ->
    %%     {Match0, Rest0} = erlang:split_binary(Input0, Length0),
    %%     {{Success(Match0, Rest0)}};
    %%   _ -> {{Fail}}
    %% end
    Regexp = abstract(R),
    MatchName = variable(new_name("Match")),
    LengthName = variable(new_name("Length")),
    RestName = variable(new_name("Input")),
    SuccessBranch = Success(MatchName, RestName),
    FailBranch = Fail(InputName, error_reason({regexp, R})),
    ?Q(["case re:run(_@InputName, _@Regexp) of ",
        "    {Match, [{0, _@LengthName}|_]} -> ",
        "        {_@MatchName, _@RestName} = erlang:split_binary(_@InputName, _@LengthName),",
        "        _@SuccessBranch; ",
        "    _ -> _@FailBranch ",
        "end"]);

generate(#string{string=S}, InputName, Success, Fail) ->
    %% Template:
    %% String = {{S}},
    %% case Input0 of
    %%    <<{{S}}/binary, Rest0/binary>> -> {{Success(S, 'Rest0')}};
    %%    _ -> {{Fail}}
    %% end
    Literal = abstract(S),
    Size = merl:term(byte_size(S)),
    StringName = variable(new_name("String")),
    RestName = variable(new_name("Input")),
    SuccessBranch = Success(Literal, RestName),
    FailBranch = Fail(InputName, error_reason({string, S})),
    ?Q(["begin ",
        "_@StringName = _@Literal, ",
        "case _@InputName of ",
        "    <<_@StringName:_@Size/binary, _@RestName/binary>> -> _@SuccessBranch; ",
        "    _ -> _@FailBranch ",
        "end ",
        "end"]);

generate(#epsilon{}, InputName, Success, _Fail) ->
    %% Passes through to Success, because epsilon always succeeds.
    %%
    %% TODO: Do we need to create a capture here? This doesn't feel right.
    %%
    %% However, epsilon will always be on the end of repetitions or as
    %% the result of an optional, so returning or consing onto [] is
    %% always ok.
    Success(?Q("[]"), InputName);

generate(#anything{}, InputName, Success, Fail) ->
    %% TODO: accept non-utf8 characters based on grammar settings.
    %%
    %% Template:
    %%
    %% case Input0 of
    %%     <<>> ->
    %%         {{Fail(anything)}};
    %%     <<Char0/utf8, Rest0/binary>> ->
    %%         {{Success('Char0', 'Rest0')}}
    %% end
    CharName = variable(new_name("Char")),
    RestName = variable(new_name("Input")),
    FailBranch = Fail(InputName, error_reason(anything)),
    SuccessBranch = Success(CharName, RestName),
    ?Q(["case _@InputName of ",
        "  <<>> -> _@FailBranch; ",
        "  <<_@CharName/utf8, _@RestName/binary>> -> _@SuccessBranch ",
        "end"]).


%% @doc Parsing error reasons
error_reason({regexp, Literal}) ->
    ?FMT("expected text matching pattern '~s'", [Literal]);
error_reason({string, Literal}) ->
    ?FMT("expected '~s'", [Literal]);
error_reason(anything) ->
    "expected any character but reached end of input".

%% @doc Unique name generator, combining a prefix with an integer
%% tracked in the process dictionary. Should be safe for variable
%% names and function names.
-spec new_name(string()) -> string().
new_name(Key) ->
    Incr = case get({namegen, Key}) of
               undefined ->
                   put({namegen, Key}, 0),
                   0;
               Int ->
                   put({namegen, Key}, Int+1),
                   Int + 1
           end,
    lists:flatten([Key, $_, integer_to_list(Incr)]).
