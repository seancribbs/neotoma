-module(test_memoization).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

setup_memo_test() ->
    peg_includes:setup_memo(),
    ?assertNot(undefined == ets:info(get(parse_memo_table))),
    peg_includes:release_memo().

release_memo_test() ->
    peg_includes:setup_memo(),
    peg_includes:release_memo(),
    ?assertEqual(undefined, ets:info(get(parse_memo_table))).

step_memo_test() ->
    peg_includes:setup_memo(),
    Result = peg_includes:p(<<"abcdefghi">>, {{line,1},{column,1}}, anything, peg_includes:p_anything()),
    ?assertEqual({<<"a">>, <<"bcdefghi">>, {{line,1},{column,2}}}, Result),
    Result2 = peg_includes:p(<<"abcdefghi">>, {{line,1},{column,1}}, anything, fun(_) ->
                                                                                  throw(bork) end),
    ?assertEqual(Result, Result2),
    peg_includes:release_memo().

concurrent_memo_test() ->
    Me = self(),
    Him = spawn(fun() -> Me ! peg_includes:setup_memo(), receive _ -> ok after 10000 -> ok end end),
    MyTid = peg_includes:setup_memo(),
    receive
        Tid -> ?assertNot(Tid == MyTid),
               Him ! ok
    after 10000 -> ok
    end,
    peg_includes:release_memo().
