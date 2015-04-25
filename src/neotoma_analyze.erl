%% @doc <p>Performs semantic analysis on the metagrammar AST,
%% returning the AST with annotations. Error conditions are recorded
%% when:</p>
%%
%% <ul><li>A non-terminal has no reduction/rule.</li>
%%     <li>An inline code-block is not a well-formed Erlang expression</li>
%%     <li>The top-level code block is not a well-formed Erlang form list.</li>
%%     <li>A non-terminal has more than one reduction/rule.</li></ul>
%%
%% <p>Warning conditions are recorded when:</p>
%%
%% <ul><li> A rule/reduction is unused. The first rule, which serves as the
%%   grammar entry-point, is not checked. </li></ul>
%% @end
-module(neotoma_analyze).
-author("Sean Cribbs <seancribbs@gmail.com>").

-include("neotoma.hrl").

-export([file/1,
         analyze/1]).

%% @doc Open a grammar file, parse and analyze it.
-spec file(file:name()) -> {ok, #grammar{}} | {error, [{error, semantic_error() |
                                                        syntax_error()} |
                                                       {warning, semantic_warning()}]}.
file(Filename) ->
    case neotoma_parse2:file(Filename) of
        #grammar{}=G ->
            %% We got a successful grammar, so analyze it.
            analyze(G#grammar{filename=Filename});
        {fail, Reason} ->
            {error, [{error, {syntax_error, Reason}}]};
        {_AST, _Rest, Index} ->
            %% We should really make the grammar intolerant of
            %% incomplete parses, but for now we can simply report the
            %% point past which the parsing failed.
            {error, [{error, {syntax_error, {incomplete_parse, Index}}}]}
    end.

-spec analyze(#grammar{}) -> {ok, #grammar{}} | {error, [{error, semantic_error()} |
                                                         {warning, semantic_warning()}]}.
analyze(#grammar{declarations=D, code=Code}=G) ->
    {NewCode, Errors} = case check_code_block(Code, []) of
                            ok -> {Code, []};
                            #code{}=Code1 ->
                                {Code1, []};
                            Errlist when is_list(Errlist) ->
                                {Code, Errlist}
                        end,
    ST = normalize_symbol_table(lists:foldl(fun analyze_declaration/2, #symbols{}, D)),
    {ST1, Errors1} = lists:foldl(fun(Checker, State) -> Checker(State) end,
                                 {ST, Errors},
                                 [fun check_nonterminals/1,
                                  fun check_rules/1,
                                  fun check_code/1]),
    case Errors1 of
        [] ->
            {ok, G#grammar{analysis=ST1, code=NewCode}};
        _ ->
            {error, Errors1}
    end.


analyze_declaration(#declaration{name=DName, expr=Reduction,
                                 code=Code, index=Index},
                    #symbols{rules=Rules}=ST) ->
    ST1 = analyze_expression(Reduction, ST),
    ST1#symbols{rules=[{DName, Index, Code}|Rules]}.

%% Ordered-Choice expressions, recurse into each alternative.
analyze_expression(#choice{alts=Alternatives},
                   #symbols{}=ST) ->
    lists:foldl(fun analyze_expression/2, ST, Alternatives);

%% Sequences of expressions, recurse into each sub-expression.
analyze_expression(#sequence{exprs=Exprs}, #symbols{}=ST) ->
    lists:foldl(fun analyze_expression/2, ST, Exprs);

%% Primary expressions
analyze_expression(#primary{expr=Expr}, #symbols{}=ST) ->
    analyze_expression(Expr, ST);

%% Non-terminals, record the name and index
analyze_expression(#nonterminal{name=Name, index=Index},
                   #symbols{nts=NTs}=ST) ->
    ST#symbols{nts=orddict:append(Name, Index, NTs)};

%% Terminals, add the appropriate combinator
analyze_expression(T, #symbols{}=ST) when is_record(T, regexp);
                                          is_record(T, string);
                                          is_record(T, charclass);
                                          is_record(T, anything) ->
    ST.

%% The symbol table is built in an efficient manner, but things might
%% be reversed or out of order. This sorts the rules and non-terminals
%% by index-order.
normalize_symbol_table(#symbols{rules=R, nts=NTS}=ST) ->
    ST#symbols{
      %% Sort the rules by index so that the head of the list is the
      %% first rule
      rules = lists:keysort(2, R),
      %% Sort each non-terminal's value so that we get its occurrences
      %% in order.
      nts = orddict:map(fun(_, Locs) -> lists:sort(Locs) end, NTS)
     }.

%% Checks that every non-terminal has an associated rule/reduction.
check_nonterminals({#symbols{nts=NTs, rules=R}=ST, Errors}) ->
    {ST, orddict:fold(fun(NT, Indexes, Errs) ->
                             check_nonterminal(NT, Indexes, R, Errs)
                     end, Errors, NTs)}.

%% Check that an individual non-terminal has an associated
%% rule/reduction. If the reduction is missing, add an error to the
%% list.
check_nonterminal(NT, Indexes, Rules, Errors) ->
    case lists:keymember(NT, 1, Rules) of
        true -> Errors;
        false ->
            [{error, {no_reduction, {NT, Indexes}}}|Errors]
    end.

%% Checks that every rule except the root of the grammar is used.
check_rules({#symbols{nts=NTs, rules=[_Root|Rules]=AllRules}=ST, Errors}) ->
    {ST, lists:foldl(fun(Rule, Errs) ->
                             check_rule(Rule, NTs, Errs)
                     end, Errors, Rules) ++
         check_rule_uniqueness(AllRules, [])}.

%% Checks that an individual rule for a non-terminal is used by some
%% other reduction.
check_rule({Name, Index, _Code}, NTs, Errors) ->
    case orddict:is_key(Name, NTs) of
        true -> Errors;
        false -> [{warning, {unused_rule, {Name, Index}}}|Errors]
    end.

%% Checks that rules are only defined once.
check_rule_uniqueness([{Name, Index, _Code}|Rest], Errors) ->
    {Others, DupesRemoved} = lists:partition(fun({Name2, _, _}) ->
                                                     Name2 == Name
                                             end, Rest),
    case Others of
        [] ->
            check_rule_uniqueness(DupesRemoved, Errors);
        _ ->
            check_rule_uniqueness(DupesRemoved,
                                  [{error, {duplicate_rule,
                                            Name, Index,
                                            [I || {_,I,_} <- Others]}}|Errors])
    end;
check_rule_uniqueness([], Errors) -> Errors.


%% Checks that every code block is a well-formed Erlang expression and
%% annotates the code block node with the used implicit arguments.
check_code({#symbols{rules=Rules}, _Errors}=Acc) ->
    lists:foldl(fun({R, Idx, Code}, {ST, Errs}) ->
                        case check_code_block(Code, Errs) of
                            ok -> {ST, Errs};
                            #code{}=NewCode ->
                                {ST#symbols{
                                   rules=lists:keyreplace(R, 1, ST#symbols.rules, {R, Idx, NewCode})
                                  },
                                 Errs};
                            Errlist when is_list(Errlist) ->
                                {ST, Errlist}
                        end
                end, Acc, Rules).

check_code_block(undefined, _) -> ok;
check_code_block(#code{identity=true}, _) -> ok;
check_code_block(#code{code=Contents, index={{line, L},{column,C}}}=Code, Errors) ->
    Comments = erl_comment_scan:string(Contents),
    case erl_scan:string(Contents, {L,C}) of
        {error, Info, EndLocation} ->
            [{error, {Info, EndLocation}}|Errors];
        {ok, Tokens, {EL, _EC}} ->
            %% We add the dot token so that it makes a complete
            %% expression list.
            case erl_parse:parse_exprs(Tokens ++ [{dot,EL}]) of
                {ok, ExprList} ->
                    %% Find which arguments are used so they can be
                    %% applied to the generated function.
                    Vars = used_transform_variables(Tokens),
                    %% Now we annotate the code block with the captured info
                    Code#code{parsed=ExprList, comments=Comments, used_args=Vars};
                {error, Reason} ->
                    [{error, Reason}|Errors]
            end
    end.

used_transform_variables(Tokens) ->
    ordsets:to_list(lists:foldl(fun used_transform_variables/2,
                                ordsets:new(), Tokens)).

used_transform_variables({var, _, 'Node'}, Acc) -> ordsets:add_element('Node', Acc);
used_transform_variables({var, _, 'Idx'}, Acc) -> ordsets:add_element('Idx', Acc);
used_transform_variables(_, Acc) -> Acc.
