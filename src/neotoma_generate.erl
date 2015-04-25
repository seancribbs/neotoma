%% @doc Implements code-generation using syntax_tools.
-module(neotoma_generate).
-include("neotoma.hrl").
-compile(export_all).
-import(erl_syntax, [
                     abstract/1,
                     add_ann/2,
                     application/2,
                     application/3,
                     atom/1,
                     %% attribute/1,
                     attribute/2,
                     binary/1,
                     binary_field/2,
                     block_expr/1,
                     block_expr_body/1,
                     case_expr/2,
                     clause/3,
                     cons/2,
                     form_list/1,
                     fun_expr/1,
                     function/2,
                     list/2,
                     match_expr/2,
                     tuple/1,
                     type/1,
                     underscore/0,
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
    function(atom(Name),
             [clause([InputName],
                     none,
                     [ExprCode])]);

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
                   [application(AltName, [IV])]
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
    block_expr([
                %% NOTE: Added annotation here so it can be
                %% potentially unrolled later
                add_ann(choice_fun,
                        match_expr(AltName,
                                   fun_expr([clause([NewInputName],
                                                    none,
                                                    FunContents)]))),
                FirstAlt
               ]);

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

generate(#primary{expr=E, label=undefined, modifier=undefined}, InputName, Success, Fail) ->
    %% Where there is no label or modifier (or we've already applied
    %% them), just generate the nested expression.
    generate(E, InputName, Success, Fail);

generate(#primary{label=L, modifier=undefined}=P, InputName, Success0, Fail) ->
    %% When a label is defined and there's no modifier (or we've
    %% already applied it), we wrap the capture in a tuple with the
    %% label.
    Success = fun(Capture, Rest) ->
                      Success0(tuple([atom(L), Capture]), Rest)
              end,
    generate(P#primary{label=undefined}, InputName, Success, Fail);

generate(#primary{modifier=optional}=P, InputName, Success, _Fail0) ->
    %% If the expression is optional, failure is also success!
    Fail = fun(InputVar, _Reason) ->
                   Success(abstract([]), InputVar)
           end,
    generate(P#primary{modifier=undefined}, InputName, Success, Fail);

generate(#primary{modifier=assert}=P, InputName, Success0, Fail) ->
    %% Zero-width positive lookahead. Failed match still fails, but a
    %% successful match does not advance the input.
    Success = fun(_Capture, _Rest) ->
                      Success0(abstract([]), InputName)
              end,
    generate(P#primary{modifier=undefined}, InputName, Success, Fail);

generate(#primary{modifier=deny}=P, InputName, Success0, Fail0) ->
    %% Zero-width negative lookahead. Successful match fails, failed
    %% match succeeds but consumes nothing.
    Success = fun(_Capture, _Rest) ->
                      %% TODO: better error reason here
                      Fail0(InputName, deny)
              end,
    Fail = fun(InputVar, _Reason) ->
                   Success0(abstract([]), InputVar)
           end,
    generate(P#primary{modifier=undefined}, InputName, Success, Fail);


generate(#nonterminal{name=NT}, InputName, Success, Fail) ->
    %% Template:
    %%
    %% case {{NT}}(???) of
    %%   {fail, Reason0} -> {{Fail}};
    %%   {ok, Result0, Rest0} -> {{Success(Result0, Rest0)}}
    %% end
    ResultName = variable(new_name("Result")),
    RestName = variable(new_name("Input")),
    ReasonName = variable(new_name("Reason")),
    case_expr(application(atom(NT), [InputName]),
              [clause([tuple([atom("fail"), ReasonName])], none, Fail(InputName, ReasonName)),
               clause([tuple([atom("ok"), ResultName, RestName])], none, Success(ResultName, RestName))]);

generate(#charclass{charclass=C, index=I}, InputName, Success, Fail) ->
    %% TODO: For now, treating character class as a regexp. In the
    %% future, this can be exploded into a more efficient case
    %% statement.
    %% BUG: the charclass has the brackets already stripped out! We
    %% should reinsert them.
    generate(#regexp{regexp=C, index=I}, InputName, Success, Fail);

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
    case_expr(application(atom("re"), atom("run"), [InputName, Regexp]),
              [clause([tuple([atom("match"), list([tuple([abstract(0), LengthName])], underscore())])], none,
                      [match_expr(tuple([MatchName, RestName]), application(atom("erlang"), atom("split_binary"), [InputName, LengthName]))
                       | Success(MatchName, RestName)]),
               clause([underscore()], none, Fail(InputName, error_reason({regexp, R})))]);

generate(#string{string=S}, InputName, Success, Fail) ->
    %% Template:
    %%
    %% case Input0 of
    %%    <<{{S}}/binary, Rest0/binary>> -> {{Success(S, 'Rest0')}};
    %%    _ -> {{Fail}}
    %% end
    Literal = abstract(S),
    RestName = variable(new_name("Input")),
    case_expr(InputName,
              [clause([binary([binary_field(Literal, [atom("binary")]),
                               binary_field(RestName, [atom("binary")])])],
                      none,
                      Success(Literal, RestName)),
               clause([underscore()], none,
                      Fail(InputName, error_reason({string, S})))]);

generate(#epsilon{}, InputName, Success, _Fail) ->
    %% Passes through to Success, because epsilon always succeeds.
    %%
    %% TODO: Do we need to create a capture here? This doesn't feel right.
    %%
    %% However, epsilon will always be on the end of repetitions or as
    %% the result of an optional, so returning or consing onto [] is
    %% always ok.
    Success(abstract([]), InputName);

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
    case_expr(InputName,
              [clause([abstract(<<>>)], none, Fail(InputName, error_reason(anything))),
               clause([binary([
                              binary_field(CharName, [atom("utf8")]),
                              binary_field(RestName, [atom("binary")])
                             ])],
                      none, Success(CharName, RestName))
               ]).


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
