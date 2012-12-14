-module(neotoma_util_tests).
-include_lib("eunit/include/eunit.hrl").

% Test the parser-combinators in the 'neotoma_util' module
-define(STARTINDEX, {{line,1},{column,1}}).
eof_test_() ->
    [
     ?_assertEqual({fail,{expected,eof,?STARTINDEX}}, (neotoma_util:p_eof())(<<"abc">>,?STARTINDEX)),
     ?_assertEqual({eof, [], ?STARTINDEX}, (neotoma_util:p_eof())(<<>>,?STARTINDEX))
    ].

optional_test_() ->
    [
     ?_assertEqual({[], <<"xyz">>,?STARTINDEX}, (neotoma_util:p_optional(neotoma_util:p_string(<<"abc">>)))(<<"xyz">>,?STARTINDEX)),
     ?_assertEqual({<<"abc">>, <<"xyz">>,{{line,1},{column,4}}}, (neotoma_util:p_optional(neotoma_util:p_string(<<"abc">>)))(<<"abcxyz">>,?STARTINDEX))
    ].

not_test_() ->
    [
     ?_assertEqual({[], <<"xyzabc">>,?STARTINDEX}, (neotoma_util:p_not(neotoma_util:p_string(<<"abc">>)))(<<"xyzabc">>,?STARTINDEX)),
     ?_assertEqual({fail,{expected, {no_match, <<"abc">>}, ?STARTINDEX}}, (neotoma_util:p_not(neotoma_util:p_string(<<"abc">>)))(<<"abcxyz">>,?STARTINDEX))
    ].

assert_test_() ->
    [
     ?_assertEqual({fail,{expected, {string, <<"abc">>}, ?STARTINDEX}}, (neotoma_util:p_assert(neotoma_util:p_string(<<"abc">>)))(<<"xyzabc">>,?STARTINDEX)),
     ?_assertEqual({[], <<"abcxyz">>,?STARTINDEX}, (neotoma_util:p_assert(neotoma_util:p_string(<<"abc">>)))(<<"abcxyz">>,?STARTINDEX))
    ].

seq_test_() ->
    [
     ?_assertEqual({[<<"abc">>,<<"def">>], <<"xyz">>,{{line,1},{column,7}}}, (neotoma_util:p_seq([neotoma_util:p_string(<<"abc">>), neotoma_util:p_string(<<"def">>)]))(<<"abcdefxyz">>,?STARTINDEX)),
     ?_assertEqual({fail,{expected, {string, <<"def">>}, {{line,1},{column,4}}}}, (neotoma_util:p_seq([neotoma_util:p_string(<<"abc">>), neotoma_util:p_string(<<"def">>)]))(<<"abcxyz">>,?STARTINDEX))
    ].

choose_test_() ->
    [
     ?_assertEqual({<<"abc">>, <<"xyz">>, {{line,1},{column,4}}}, (neotoma_util:p_choose([neotoma_util:p_string(<<"abc">>), neotoma_util:p_string(<<"def">>)]))(<<"abcxyz">>,?STARTINDEX)),
     ?_assertEqual({<<"def">>, <<"xyz">>, {{line,1},{column,4}}}, (neotoma_util:p_choose([neotoma_util:p_string(<<"abc">>), neotoma_util:p_string(<<"def">>)]))(<<"defxyz">>,?STARTINDEX)),
     ?_assertEqual({<<"xyz">>, <<"xyz">>, {{line,1},{column,4}}}, (neotoma_util:p_choose([neotoma_util:p_string(<<"abc">>), neotoma_util:p_string(<<"def">>), neotoma_util:p_string(<<"xyz">>)]))(<<"xyzxyz">>,?STARTINDEX)),
     ?_assertEqual({fail,{expected,{string,<<"abc">>},?STARTINDEX}}, (neotoma_util:p_choose([neotoma_util:p_string(<<"abc">>),neotoma_util:p_string(<<"def">>)]))(<<"xyz">>, ?STARTINDEX))
    ].

zero_or_more_test_() ->
    [
     ?_assertEqual({[], <<>>, ?STARTINDEX}, (neotoma_util:p_zero_or_more(neotoma_util:p_string(<<"abc">>)))(<<"">>,?STARTINDEX)),
     ?_assertEqual({[], <<"def">>,?STARTINDEX}, (neotoma_util:p_zero_or_more(neotoma_util:p_string(<<"abc">>)))(<<"def">>,?STARTINDEX)),
     ?_assertEqual({[<<"abc">>], <<"def">>,{{line,1},{column,4}}}, (neotoma_util:p_zero_or_more(neotoma_util:p_string(<<"abc">>)))(<<"abcdef">>,?STARTINDEX)),
     ?_assertEqual({[<<"abc">>, <<"abc">>], <<"def">>,{{line,1},{column,7}}}, (neotoma_util:p_zero_or_more(neotoma_util:p_string(<<"abc">>)))(<<"abcabcdef">>,?STARTINDEX))
    ].

one_or_more_test_() ->
    [
     ?_assertEqual({fail,{expected,
                          {at_least_one,
                           {string, <<"abc">>}
                          }, ?STARTINDEX}}, (neotoma_util:p_one_or_more(neotoma_util:p_string(<<"abc">>)))(<<"def">>,?STARTINDEX)),
     ?_assertEqual({[<<"abc">>], <<"def">>,{{line,1},{column,4}}}, (neotoma_util:p_one_or_more(neotoma_util:p_string(<<"abc">>)))(<<"abcdef">>,?STARTINDEX)),
     ?_assertEqual({[<<"abc">>,<<"abc">>], <<"def">>,{{line,1},{column,7}}}, (neotoma_util:p_one_or_more(neotoma_util:p_string(<<"abc">>)))(<<"abcabcdef">>,?STARTINDEX))
    ].

label_test_() ->
    [
     ?_assertEqual({fail,{expected, {string, <<"!">>}, ?STARTINDEX}}, (neotoma_util:p_label(bang, neotoma_util:p_string(<<"!">>)))(<<"?">>,?STARTINDEX)),
     ?_assertEqual({{bang, <<"!">>}, <<"">>,{{line,1},{column,2}}}, (neotoma_util:p_label(bang, neotoma_util:p_string(<<"!">>)))(<<"!">>,?STARTINDEX))
    ].

string_test_() ->
    [
     ?_assertEqual({<<"abc">>, <<"def">>,{{line,1},{column,4}}}, (neotoma_util:p_string(<<"abc">>))(<<"abcdef">>,?STARTINDEX)),
     ?_assertEqual({fail,{expected, {string, <<"abc">>}, ?STARTINDEX}}, (neotoma_util:p_string(<<"abc">>))(<<"defabc">>,?STARTINDEX))
    ].

anything_test_() ->
    [
     ?_assertEqual({<<"a">>,<<"bcde">>,{{line,1},{column,2}}}, (neotoma_util:p_anything())(<<"abcde">>,?STARTINDEX)),
     ?_assertEqual({<<"\n">>,<<"bcde">>,{{line,2},{column,1}}}, (neotoma_util:p_anything())(<<"\nbcde">>,?STARTINDEX)),
     ?_assertEqual({fail,{expected, any_character, ?STARTINDEX}}, (neotoma_util:p_anything())(<<"">>,?STARTINDEX))
    ].

charclass_test_() ->
    [
     ?_assertEqual({<<"+">>,<<"----">>,{{line,1},{column,2}}}, (neotoma_util:p_charclass(<<"[+]">>))(<<"+----">>,?STARTINDEX)),
     ?_assertEqual({fail,{expected, {character_class, "[+]"}, ?STARTINDEX}}, (neotoma_util:p_charclass(<<"[+]">>))(<<"----">>,?STARTINDEX))
    ].

line_test() ->
    ?assertEqual(1, neotoma_util:line({{line,1},{column,2}})).

column_test() ->
    ?assertEqual(2, neotoma_util:column({{line,1},{column,2}})).

setup_memo_test() ->
    neotoma_util:setup_memo(),
    ?assertNot(undefined == ets:info(get({parse_memo_table, neotoma_util}))),
    neotoma_util:release_memo().

release_memo_test() ->
    neotoma_util:setup_memo(),
    neotoma_util:release_memo(),
    ?assertEqual(undefined, ets:info(get({parse_memo_table, neotoma_util}))).

step_memo_test() ->
    neotoma_util:setup_memo(),
    Result = neotoma_util:p(<<"abcdefghi">>, {{line,1},{column,1}}, anything, neotoma_util:p_anything()),
    ?assertEqual({<<"a">>, <<"bcdefghi">>, {{line,1},{column,2}}}, Result),
    Result2 = neotoma_util:p(<<"abcdefghi">>, {{line,1},{column,1}}, anything, fun(_) ->
                                                                                  throw(bork) end),
    ?assertEqual(Result, Result2),
    neotoma_util:release_memo().

concurrent_memo_test() ->
    Me = self(),
    Him = spawn(fun() -> Me ! neotoma_util:setup_memo(), receive _ -> ok after 10000 -> ok end end),
    MyTid = neotoma_util:setup_memo(),
    receive
        Tid -> ?assertNot(Tid == MyTid),
               Him ! ok
    after 10000 -> ok
    end,
    neotoma_util:release_memo().
