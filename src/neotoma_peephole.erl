%% @doc Implements simple local transformations to reduce rule
%% complexity in the grammar. {@link
%% http://bford.info/pub/lang/thesis.pdf#4.4.1}
-module(neotoma_peephole).
-include("neotoma.hrl").
-export([optimize/1]).

optimize(G=#grammar{declarations=Decls}) ->
    %% Compute a fixpoint by recursively optimizing until no other
    %% optimizations can be made.
    case lists:map(fun optimize/1, Decls) of
        Decls ->
            G#grammar{declarations=Decls};
        Optim ->
            optimize(G#grammar{declarations=Optim})
    end;
optimize(D=#declaration{expr=E}) ->
    D#declaration{expr=optimize(E)};

%% 1. Redundant sequencing operators containing only one component,
%% and choice operators containing only one alternative, are
%% eliminated.
optimize(#choice{alts=[A]}) ->
    optimize(A);
optimize(#sequence{exprs=[E]}) ->
    optimize(E);

%% 2. Sequence operators nested directly within other sequence
%% operators, and choice operators nested directly within other choice
%% operators, are "flattened". For example, r1/(r2/r3)/r4 is rewritten
%% r1/r2/r3/r4.
optimize(C=#choice{alts=Alts}) ->
    NewAlts = lists:foldr(flattener(choice, #choice.alts), [], Alts),
    C#choice{alts=NewAlts};
optimize(S=#sequence{exprs=Exprs}) ->
    NewExprs = lists:foldr(flattener(sequence, #sequence.exprs), [], Exprs),
    S#sequence{exprs=NewExprs};
optimize(P=#primary{expr=E}) ->
    P#primary{expr=optimize(E)};
optimize(Other) ->
    Other.

%% Flattens redundant choice or sequence operators for optimization 2.
flattener(Tag, Field) ->
    fun(Tuple, Acc) when element(1, Tuple) == Tag ->
            Nested = element(Field, Tuple),
            Optim = lists:map(fun optimize/1, Nested),
            Optim ++ Acc;
       (Other, Acc) ->
            [optimize(Other)|Acc]
    end.
