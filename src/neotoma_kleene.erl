%% @doc Optimization that expands greedy repetition (Kleene operators)
%% into recursion. Regular combinatorial approaches can violate the
%% linear-time parsing guarantee, whereas recursion can be memoized,
%% ensuring linear time. {@link
%% http://bford.info/pub/lang/thesis.pdf#4.2.2}
-module(neotoma_kleene).
-include("neotoma.hrl").
-export([transform/1]).

transform(G=#grammar{declarations=Decls}) ->
    G#grammar{declarations=transform_declarations(Decls)}.

transform_declarations(Decls) ->
    {Xformed, Expansions, _NewCount} =
        lists:foldr(fun transform_declaration/2,
                    {[], [], 0},
                    Decls),
    lists:keysort(#declaration.index, Xformed ++ dedupe_expansions(Expansions)).

transform_declaration(#declaration{expr=E}=D, {Decls, Expansions, Count}) ->
    {NewExpr, NewRules, NewCount} = transform_expression(E, undefined, Count),
    {[D#declaration{expr=NewExpr}|Decls], NewRules ++ Expansions, NewCount}.


%% @doc Recurses through the RHS of declarations/rules and expands
%% repetition into recursion, naming new rules as it goes along.
transform_expression(#plus{expr=E, index=I}=P, Label, Count) ->
    %% LetterOrDigitPlus :: {[Char]} =
    %%     c:LetterOrDigit cs:LetterOrDigitPlus -> {c : cs}
    %%     / c:LetterOrDigit -> {[c]}
    {SubExpr, SubRules, SubCount} = transform_expression(E, undefined, Count),
    {NewName, NewCount} = name_expansion(P, Label, SubCount),
    NewNT = #nonterminal{name=NewName, index=I},
    Reduction = #choice{alts=[#sequence{exprs=[SubExpr, NewNT], index=I},
                              SubExpr],
                        index=I},
    Rule = #declaration{name=NewName, expr=Reduction, code=#code{identity=true}, index=I},
    {NewNT, [Rule|SubRules], NewCount};
transform_expression(#star{expr=E, index=I}=P, Label, Count) ->
    %% LetterOrDigitStar :: {[Char]} =
    %%     c:LetterOrDigit cs:LetterOrDigitStar -> {c : cs}
    %%     / -> {[]}
    {SubExpr, SubRules, SubCount} = transform_expression(E, undefined, Count),
    {NewName, NewCount} = name_expansion(P, Label, SubCount),
    NewNT = #nonterminal{name=NewName, index=I},
    Reduction = #choice{alts=[#sequence{exprs=[SubExpr, NewNT], index=I},
                              #epsilon{index=I}],
                        index=I},
    Rule = #declaration{name=NewName, expr=Reduction, code=#code{identity=true}, index=I},
    {NewNT, [Rule|SubRules], NewCount};

transform_expression(#label{expr=E, label=L}=Primary, _Label, Count) when ?IS_REPETITION(E) ->
    {NewExpr, NewRules, NewCount} = transform_expression(E, L, Count),
    {?SET_EXPR(Primary, NewExpr), NewRules, NewCount};


transform_expression(Primary, Label, Count) when ?IS_PRIMARY(Primary) ->
    {NewExpr, NewRules, NewCount} = transform_expression(?PRIMARY_EXPR(Primary), Label, Count),
    {?SET_EXPR(Primary, NewExpr), NewRules, NewCount};

transform_expression(#sequence{exprs=E}=S, Label, Count) ->
    {NewExprs, NewRules, NewCount} = lists:foldr(
                                       fun(Expr, {Exprs, Rules, C}) ->
                                               {SubExpr, SubRules, SubCount} = transform_expression(Expr, Label, C),
                                               {[SubExpr|Exprs], SubRules ++ Rules, SubCount}
                                       end,
                                       {[], [], Count},
                                       E),
    {S#sequence{exprs=NewExprs}, NewRules, NewCount};

transform_expression(#choice{alts=E}=Choice, Label, Count) ->
    {NewExprs, NewRules, NewCount} = lists:foldr(
                                       fun(Expr, {Exprs, Rules, C}) ->
                                               {SubExpr, SubRules, SubCount} = transform_expression(Expr, Label, C),
                                               {[SubExpr|Exprs], SubRules ++ Rules, SubCount}
                                       end,
                                       {[], [], Count},
                                       E),
    {Choice#choice{alts=NewExprs}, NewRules, NewCount};

transform_expression(TermOrNonTerm, _Label, Count) ->
    {TermOrNonTerm, [], Count}.

%% @doc Derive a name for the repetition expansion. If it is a single
%% nonterminal, use the nonterminal's name. If it is a terminal, use
%% the type of terminal and the count. Otherwise, derive a new name
%% from the count.
%% @end

%% TODO: consider how to pass contextual names down the stack, e.g. labels or rule names
-spec name_expansion(#plus{} | #star{}, atom(), non_neg_integer()) -> {atom(), non_neg_integer()}.
%% '.' have always the same expansion, optimize them out
name_expansion(#plus{expr=#anything{}}, _Label, Count) ->
    {anything_plus, Count};
name_expansion(#star{expr=#anything{}}, _Label, Count) ->
    {anything_star, Count};

%% Expansions of the same nonterminal will have the same name
name_expansion(M, _Label, Count) when ?IS_REPETITION(M), is_record(?PRIMARY_EXPR(M), nonterminal)->
    #nonterminal{name=N} = ?PRIMARY_EXPR(M),
    IdStr = unicode:characters_to_binary([atom_to_binary(N, utf8), $_, modifier_name(M)]),
    {binary_to_atom(IdStr, utf8), Count};

%% If a label immediately wraps this repetition, use its name.
name_expansion(M, Label, Count) when Label /= undefined ->
    IdStr = unicode:characters_to_binary([atom_to_binary(Label, utf8), $_,
                                          modifier_name(M), $_,
                                          integer_to_list(Count)]),
    {binary_to_atom(IdStr, utf8), Count+1};

%% When literal strings, regexps or charclasses are repeated, use the
%% name of the syntactic element for the repetition.
name_expansion(M, _Label, Count) when ?IS_LITERAL(?PRIMARY_EXPR(M)) ->
    E = ?PRIMARY_EXPR(M),
    IdStr = unicode:characters_to_binary([atom_to_list(element(1, E)),
                                          $_, modifier_name(M), $_,
                                          integer_to_list(Count)]),
    {binary_to_atom(IdStr, utf8), Count+1};

%% Anything else (sequence, choice), just generate a name based on the
%% repetition operator.
name_expansion(M, _Label, Count) when ?IS_REPETITION(M) ->
    IdStr = unicode:characters_to_binary([modifier_name(M), $_, integer_to_list(Count)]),
    {binary_to_atom(IdStr, utf8), Count+1}.


-spec modifier_name(#plus{} | #star{}) -> binary().
modifier_name(#plus{}) -> <<"plus">>;
modifier_name(#star{}) -> <<"star">>.

dedupe_expansions(Rules) ->
    lists:foldr(fun dedupe_expansion/2, [], Rules).

dedupe_expansion(#declaration{name=Name, index=Idx}=Decl,
                 Acc) ->
    %% TODO: This could be done more cleanly
    case lists:keytake(Name, #declaration.name, Acc) of
        false -> [Decl|Acc];
        {value, Other, NewAcc} ->
            if Other#declaration.index >= Idx ->
                    [Decl|NewAcc];
               true ->
                    Acc
            end
    end.
