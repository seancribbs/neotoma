-module(test_memoization).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

setup_memo_test() ->
  peg:setup_memo(),
  ?assertNot(undefined == get(ets_table)),
  ?assertMatch([[{current_index, 0}]], ets:match(get(ets_table), '$1')).

release_memo_test() ->
  peg:setup_memo(),
  Tid = get(ets_table),
  peg:release_memo(),
  ?assertEqual(undefined, get(ets_table)),
  ?assertEqual(undefined, ets:info(Tid)).
