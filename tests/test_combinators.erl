-module(test_combinators).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

% Test the parser-combinators in the 'peg' module

eof_test_() ->
  [
   ?_assertEqual(fail, (peg:eof())("abc")),
   ?_assertEqual({eof, []}, (peg:eof())(""))
  ].

optional_test_() ->
  [
   ?_assertEqual({[], "xyz"}, (peg:optional(peg:string("abc")))("xyz")),
   ?_assertEqual({"abc", "xyz"}, (peg:optional(peg:string("abc")))("abcxyz"))
  ].

not_test_() ->
  [
   ?_assertEqual({[], "xyzabc"}, (peg:not_(peg:string("abc")))("xyzabc")),
   ?_assertEqual(fail, (peg:not_(peg:string("abc")))("abcxyz"))
  ].

assert_test_() ->
  [
   ?_assertEqual(fail, (peg:assert(peg:string("abc")))("xyzabc")),
   ?_assertEqual({[], "abcxyz"}, (peg:assert(peg:string("abc")))("abcxyz"))
  ].

seq_test_() ->
  [
   ?_assertEqual({["abc","def"], "xyz"}, (peg:seq([peg:string("abc"), peg:string("def")]))("abcdefxyz")),
   ?_assertEqual(fail, (peg:seq([peg:string("abc"), peg:string("def")]))("abcxyz"))
  ].

choose_test_() ->
  [
   ?_assertEqual({"abc", "xyz"}, (peg:choose([peg:string("abc"), peg:string("def")]))("abcxyz")),
   ?_assertEqual({"def", "xyz"}, (peg:choose([peg:string("abc"), peg:string("def")]))("defxyz")),
   ?_assertEqual({"xyz", "xyz"}, (peg:choose([peg:string("abc"), peg:string("def"), peg:string("xyz")]))("xyzxyz"))
  ].

zero_or_more_test_() ->
  [
   ?_assertEqual({[], []}, (peg:zero_or_more(peg:string("abc")))("")),
   ?_assertEqual({[], "def"}, (peg:zero_or_more(peg:string("abc")))("def")),
   ?_assertEqual({["abc"], "def"}, (peg:zero_or_more(peg:string("abc")))("abcdef")),
   ?_assertEqual({["abc", "abc"], "def"}, (peg:zero_or_more(peg:string("abc")))("abcabcdef"))
  ].

one_or_more_test_() ->
  [
   ?_assertEqual(fail, (peg:one_or_more(peg:string("abc")))("def")),
   ?_assertEqual({["abc"], "def"}, (peg:one_or_more(peg:string("abc")))("abcdef")),
   ?_assertEqual({["abc","abc"], "def"}, (peg:one_or_more(peg:string("abc")))("abcabcdef"))
  ].

label_test_() ->
  [
   ?_assertEqual(fail, (peg:label(bang, peg:string("!")))("?")),
   ?_assertEqual({{bang, "!"}, ""}, (peg:label(bang, peg:string("!")))("!"))
  ].

string_test_() ->
  [
   ?_assertEqual({"abc", "def"}, (peg:string("abc"))("abcdef")),
   ?_assertEqual(fail, (peg:string("abc"))("defabc"))
  ].

anything_test_() ->
  [
   ?_assertEqual({$a,"bcde"}, (peg:anything())("abcde")),
   ?_assertEqual(fail, (peg:anything())(""))
  ].

charclass_test_() ->
  [
   ?_assertEqual({$+,"----"}, (peg:charclass("[+]"))("+----")),
   ?_assertEqual(fail, (peg:charclass("[+]"))("----"))
  ].
