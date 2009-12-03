-module(test_memoization).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").
-define(TABLE, list_to_atom("neotoma_peg" ++ pid_to_list(self()))).

setup_memo_test() ->
  neotoma_peg:setup_memo(),
  ?assertNot(undefined == ets:info(?TABLE)),
  neotoma_peg:release_memo().

release_memo_test() ->
  neotoma_peg:setup_memo(),
  neotoma_peg:release_memo(),
  ?assertEqual(undefined, ets:info(?TABLE)).

step_memo_test() ->
  neotoma_peg:setup_memo(),
  Result = neotoma_peg:p("abcdefghi", {{line,1},{column,1}}, anything, neotoma_peg:p_anything()),
  ?assertEqual({$a, "bcdefghi", {{line,1},{column,2}}}, Result),
  Result2 = neotoma_peg:p("abcdefghi", {{line,1},{column,1}}, anything, fun(_) ->
                                             throw(bork) end),
  ?assertEqual(Result, Result2),
  neotoma_peg:release_memo().

concurrent_memo_test() ->
  spawn(fun() -> ?assertMatch(Tid when is_atom(Tid), neotoma_peg:setup_memo()) end),
  ?assertMatch(Tid2 when is_atom(Tid2), neotoma_peg:setup_memo()),
  neotoma_peg:release_memo().
