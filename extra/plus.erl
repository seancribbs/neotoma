-module(plus).
-include("../include/peg.hrl").

?root(plus_or_minus).

rule(plus_or_minus) ->
  peg:seq([peg:choose([fun plus/1, fun minus/1]), fun peg:eof/1]);
rule(plus) ->
  peg:zero_or_more(peg:string("+"));
rule(minus) ->
  peg:zero_or_more(peg:string("-")).