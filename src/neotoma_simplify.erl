%% @doc Implements simple local transformations to reduce rule
%% complexity in the grammar. {@link
%% http://bford.info/pub/lang/thesis.pdf#4.4.1}
-module(neotoma_simplify).
-include("neotoma.hrl").
-export([simplify/1]).

simplify(G=#grammar{declarations=Decls}) ->
    %% Compute a fixpoint by recursively optimizing until no other
    %% optimizations can be made.
    Peepholed = lists:map(fun peephole/1, Decls),
    %% Collapsed = collapse(Peepholed),
    if
        %% Collapsed == Decls ->
        Peepholed == Decls ->
            G;
       true ->
            %% simplify(G#grammar{declarations=Collapsed})
            simplify(G#grammar{declarations=Peepholed})
    end.

peephole(D=#declaration{expr=E}) ->
    D#declaration{expr=peephole(E)};

%% 1. Redundant sequencing operators containing only one component,
%% and choice operators containing only one alternative, are
%% eliminated.
peephole(#choice{alts=[A]}) ->
    peephole(A);
peephole(#sequence{exprs=[E]}) ->
    peephole(E);

%% 2. Sequence operators nested directly within other sequence
%% operators, and choice operators nested directly within other choice
%% operators, are "flattened". For example, r1/(r2/r3)/r4 is rewritten
%% r1/r2/r3/r4.
peephole(C=#choice{alts=Alts}) ->
    NewAlts = lists:foldr(flattener(choice, #choice.alts), [], Alts),
    C#choice{alts=NewAlts};
peephole(S=#sequence{exprs=Exprs}) ->
    NewExprs = lists:foldr(flattener(sequence, #sequence.exprs), [], Exprs),
    S#sequence{exprs=NewExprs};
peephole(Record) when ?IS_PRIMARY(Record) ->
    ?SET_EXPR(Record, peephole(?PRIMARY_EXPR(Record)));
peephole(Other) ->
    Other.

%% Flattens redundant choice or sequence operators for optimization 2.
flattener(Tag, Field) ->
    fun(Tuple, Acc) when element(1, Tuple) == Tag ->
            Nested = element(Field, Tuple),
            Optim = lists:map(fun peephole/1, Nested),
            Optim ++ Acc;
       (Other, Acc) ->
            [peephole(Other)|Acc]
    end.

%% -spec collapse([#declaration{}]) -> {ok, [#declaration{}]} | none.
%% collapse([Root|Decls]=All) ->
%%     case collapse(Decls, Root) of
%%         [] -> All;
%%         Elims ->


%% collapse(Decls, Root) ->

