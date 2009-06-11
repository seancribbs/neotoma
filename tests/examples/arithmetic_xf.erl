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

transform(decimal, Node) ->
  list_to_integer([Node]);
transform(primary, Node) when is_integer(Node) ->
  Node;
transform(primary, Node) when is_list(Node) ->
  lists:nth(2, Node);
transform(multitive, Node) when is_integer(Node) ->
  Node;
transform(multitive, Node) when is_list(Node) ->
  hd(Node) * lists:nth(3, Node);
transform(additive, Node) when is_integer(Node) ->
  Node;
transform(additive, Node) when is_list(Node) ->
  hd(Node) + lists:nth(3, Node).
