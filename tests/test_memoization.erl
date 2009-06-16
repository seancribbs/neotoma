-module(test_memoization).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

setup_memo_test() ->
  peg:setup_memo(?MODULE),
  ?assertNot(undefined == get(ets_table)).

release_memo_test() ->
  peg:setup_memo(?MODULE),
  Tid = get(ets_table),
  peg:release_memo(),
  ?assertEqual(undefined, get(ets_table)),
  ?assertEqual(undefined, ets:info(Tid)).

step_memo_test() ->
  peg:setup_memo(?MODULE),
  Result = peg:p("abcdefghi", 0, anything, peg:anything()),
  ?assertEqual({$a, "bcdefghi", 1}, Result),
  Result2 = peg:p("abcdefghi", 0, anything, fun(_) ->
                                             throw(bork) end),
  ?assertEqual(Result, Result2).
