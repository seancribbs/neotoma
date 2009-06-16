-module(test_combinators).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

% Test the parser-combinators in the 'peg' module

eof_test_() ->
  [
   ?_assertEqual({fail,0}, (peg:eof())("abc",0)),
   ?_assertEqual({eof, [], 0}, (peg:eof())("",0))
  ].

optional_test_() ->
  [
   ?_assertEqual({[], "xyz",0}, (peg:optional(peg:string("abc")))("xyz",0)),
   ?_assertEqual({"abc", "xyz",3}, (peg:optional(peg:string("abc")))("abcxyz",0))
  ].

not_test_() ->
  [
   ?_assertEqual({[], "xyzabc",0}, (peg:not_(peg:string("abc")))("xyzabc",0)),
   ?_assertEqual({fail,0}, (peg:not_(peg:string("abc")))("abcxyz",0))
  ].

assert_test_() ->
  [
   ?_assertEqual({fail,0}, (peg:assert(peg:string("abc")))("xyzabc",0)),
   ?_assertEqual({[], "abcxyz",0}, (peg:assert(peg:string("abc")))("abcxyz",0))
  ].

seq_test_() ->
  [
   ?_assertEqual({["abc","def"], "xyz",6}, (peg:seq([peg:string("abc"), peg:string("def")]))("abcdefxyz",0)),
   ?_assertEqual({fail,3}, (peg:seq([peg:string("abc"), peg:string("def")]))("abcxyz",0))
  ].

choose_test_() ->
  [
   ?_assertEqual({"abc", "xyz", 3}, (peg:choose([peg:string("abc"), peg:string("def")]))("abcxyz",0)),
   ?_assertEqual({"def", "xyz", 3}, (peg:choose([peg:string("abc"), peg:string("def")]))("defxyz",0)),
   ?_assertEqual({"xyz", "xyz", 3}, (peg:choose([peg:string("abc"), peg:string("def"), peg:string("xyz")]))("xyzxyz",0))
  ].

zero_or_more_test_() ->
  [
   ?_assertEqual({[], [], 0}, (peg:zero_or_more(peg:string("abc")))("",0)),
   ?_assertEqual({[], "def",0}, (peg:zero_or_more(peg:string("abc")))("def",0)),
   ?_assertEqual({["abc"], "def",3}, (peg:zero_or_more(peg:string("abc")))("abcdef",0)),
   ?_assertEqual({["abc", "abc"], "def",6}, (peg:zero_or_more(peg:string("abc")))("abcabcdef",0))
  ].

one_or_more_test_() ->
  [
   ?_assertEqual({fail,0}, (peg:one_or_more(peg:string("abc")))("def",0)),
   ?_assertEqual({["abc"], "def",3}, (peg:one_or_more(peg:string("abc")))("abcdef",0)),
   ?_assertEqual({["abc","abc"], "def",6}, (peg:one_or_more(peg:string("abc")))("abcabcdef",0))
  ].

label_test_() ->
  [
   ?_assertEqual({fail,0}, (peg:label(bang, peg:string("!")))("?",0)),
   ?_assertEqual({{bang, "!"}, "",1}, (peg:label(bang, peg:string("!")))("!",0))
  ].

string_test_() ->
  [
   ?_assertEqual({"abc", "def",3}, (peg:string("abc"))("abcdef",0)),
   ?_assertEqual({fail,0}, (peg:string("abc"))("defabc",0))
  ].

anything_test_() ->
  [
   ?_assertEqual({$a,"bcde",1}, (peg:anything())("abcde",0)),
   ?_assertEqual({fail,0}, (peg:anything())("",0))
  ].

charclass_test_() ->
  [
   ?_assertEqual({$+,"----",1}, (peg:charclass("[+]"))("+----",0)),
   ?_assertEqual({fail,0}, (peg:charclass("[+]"))("----",0))
  ].
