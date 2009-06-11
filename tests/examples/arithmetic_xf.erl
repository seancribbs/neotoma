-module(arithmetic_xf).
-include("../../include/peg.hrl").

rule(additive) ->
  peg:choose([peg:seq([fun multitive/1,
                       peg:string("+"),
                       fun additive/1]),
              fun multitive/1]);

rule(multitive) ->
  peg:choose([peg:seq([fun primary/1,
                       peg:string("*"),
                       fun multitive/1]),
              fun primary/1]);

rule(primary) ->
  peg:choose([peg:seq([peg:string("("),
                       fun additive/1,
                       peg:string(")")]),
              fun decimal/1]);

rule(decimal) ->
  peg:charclass("[0-9]").
