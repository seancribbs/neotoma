-module(test_memoization).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

setup_memo_test() ->
  peg:setup_memo(),
  ?assertNot(undefined == ets:info(peg)),
  peg:release_memo().

release_memo_test() ->
  peg:setup_memo(),
  peg:release_memo(),
  ?assertEqual(undefined, ets:info(peg)).

step_memo_test() ->
  peg:setup_memo(),
  Result = peg:p("abcdefghi", {{line,1},{column,1}}, anything, peg:p_anything()),
  ?assertEqual({$a, "bcdefghi", {{line,1},{column,2}}}, Result),
  Result2 = peg:p("abcdefghi", {{line,1},{column,1}}, anything, fun(_) ->
                                             throw(bork) end),
  ?assertEqual(Result, Result2),
  peg:release_memo().
