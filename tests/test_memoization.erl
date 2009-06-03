-module(test_memoization).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

setup_memo_test() ->
  peg:setup_memo(?MODULE),
  ?assertNot(undefined == get(ets_table)),
  ?assertMatch([[{current_index, 0}]], ets:match(get(ets_table), '$1')).

release_memo_test() ->
  peg:setup_memo(?MODULE),
  Tid = get(ets_table),
  peg:release_memo(),
  ?assertEqual(undefined, get(ets_table)),
  ?assertEqual(undefined, ets:info(Tid)).

step_memo_test() ->
  peg:setup_memo(?MODULE),
  Result = peg:p("abcdefghi", anything, peg:anything()),
  ?assertEqual({$a, "bcdefghi"}, Result),
  ets:insert(get(ets_table), {current_index, 0}), % Reset the index to the beginning
  Result2 = peg:p("abcdefghi", anything, fun(_) ->
                                             throw(bork) end),
  ?assertEqual(Result, Result2).
