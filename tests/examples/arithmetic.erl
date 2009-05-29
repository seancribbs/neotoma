-module(arithmetic).
-export([parse/1]).

parse(Input) ->
  peg:setup_memo(?MODULE),
  Result = case peg:p(Input, additive, fun additive/1) of
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
                                         fun multitive/1]))(I) end).

multitive(Input) ->
  peg:p(Input, multitive, fun(I) ->
                              (peg:choose([peg:seq([fun primary/1,
                                                   peg:string("*"),
                                                   fun multitive/1]),
                                          fun primary/1]))(I)
                          end).

primary(Input) ->
  peg:p(Input, primary, fun(I) ->
                            (peg:choose([peg:seq([peg:string("("),
                                                 fun additive/1,
                                                 peg:string(")")]),
                                        fun decimal/1]))(I)
                        end).

decimal(Input) ->
  peg:p(Input, decimal, fun(I) ->
                            (peg:charclass("[0-9]"))(I)
                        end).
