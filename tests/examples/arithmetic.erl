-module(arithmetic).
-export([parse/1]).
-include("../../include/peg.hrl").

parse(Input) ->
  peg:setup_memo(arithmetic),
  Result = case additive(Input) of
             {AST, []} ->
                AST;
             fail -> fail
           end,
  peg:release_memo(),
  Result.

additive(Input) ->
  peg:p(Input, additive, fun(I) ->
                             (peg:choose([peg:seq([fun multitive/1,
                                                  peg:string("+"),
                                                  fun additive/1]),
                                         fun multitive/1]))(I) end,
       fun(Node) -> transform(additive, Node) end).

multitive(Input) ->
  peg:p(Input, multitive, fun(I) ->
                              (peg:choose([peg:seq([fun primary/1,
                                                   peg:string("*"),
                                                   fun multitive/1]),
                                          fun primary/1]))(I)
                          end,
       fun(Node) -> transform(multitive, Node) end).

primary(Input) ->
  peg:p(Input, primary, fun(I) ->
                            (peg:choose([peg:seq([peg:string("("),
                                                 fun additive/1,
                                                 peg:string(")")]),
                                        fun decimal/1]))(I)
                        end,
       fun(Node) -> transform(primary, Node) end).

decimal(Input) ->
  peg:p(Input, decimal, fun(I) ->
                            (peg:charclass("[0-9]"))(I)
                        end,
       fun(Node) -> transform(decimal, Node) end).

%% Transform the nodes into the result of the expression
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
