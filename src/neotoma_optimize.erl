%% @doc Wrapper module for the optimization stage. Also computes cost
%% metrics so that we can tune what to optimize out.
-module(neotoma_optimize).
-include("neotoma.hrl").
-export([optimize/1]).

optimize(#grammar{}=G0) ->
    annotate_metrics(G0).


annotate_metrics(#grammar{declarations=Ds}=G) ->
    Annotated = [ D#declaration{cost=sizeof(E), calls=collect_calls(E)}
                  || D=#declaration{expr=E} <- Ds ],
    G#grammar{declarations=Annotated}.

%% @doc Recursively records the non-terminals (and how many times they
%% are called) in a given expression into an orddict.
-spec collect_calls(expression() | terminal()) -> orddict:orddict().
collect_calls(Expr) ->
    collect_calls(Expr, orddict:new()).

-spec collect_calls(expression() | terminal(), orddict:orddict()) -> orddict:orddict().
collect_calls(#nonterminal{name=N}, Dict) ->
    orddict:update_counter(N, 1, Dict);
collect_calls(#primary{expr=E}, Dict) ->
    collect_calls(E, Dict);
collect_calls(#sequence{exprs=Es}, Dict) ->
    lists:foldl(fun collect_calls/2, Dict, Es);
collect_calls(#choice{alts=As}, Dict) ->
    lists:foldl(fun collect_calls/2, Dict, As);
collect_calls(_, Dict) ->
    Dict.

%% @doc Recursively determines the size of an expression, which is
%% essentially the sum of all terminals, non-terminals and
%% higher-level constructs.
-spec sizeof(expression() | terminal()) -> non_neg_integer().
sizeof(#nonterminal{}) -> 1;
sizeof(#string{}) -> 1;
sizeof(#charclass{}) -> 1;
sizeof(#anything{}) -> 1;
sizeof(#epsilon{}) -> 0;
sizeof(#primary{expr=E, modifier=M}) when M /= undefined -> 1 + sizeof(E);
sizeof(#primary{expr=E}) -> sizeof(E);
sizeof(#sequence{exprs=Es}) -> lists:foldl(fun(E,C) -> C + sizeof(E) end, 1, Es);
sizeof(#choice{alts=As}) -> lists:foldl(fun(E,C) -> C + sizeof(E) end, 1, As).
