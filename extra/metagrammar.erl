-module(metagrammar).
-export([parse/1]).
-include("../include/peg.hrl").

rule(rules) ->
  peg:seq([peg:optional(fun space/1),
           fun declaration_sequence/1,
           peg:optional(fun space/1)]);

rule(declaration_sequence) ->
  peg:seq([
           peg:label(head, fun declaration/1),
           peg:label(tail, peg:zero_or_more(
             peg:seq([
                      fun space/1,
                      fun declaration/1])))]);

rule(declaration) ->
  peg:seq([
           fun nonterminal/1,
           fun space/1,
           peg:string("<-"),
           fun space/1,
           fun parsing_expression/1,
           peg:optional(fun space/1),
           peg:string(";")
          ]);

rule(parsing_expression) ->
  peg:choose([
              fun choice/1,
              fun sequence/1,
              fun primary/1
             ]);

rule(choice) ->
  peg:seq([
           peg:label(head, fun alternative/1),
           peg:label(tail, peg:one_or_more(
             peg:seq([
                      fun space/1,
                      peg:string("/"),
                      fun space/1,
                      fun alternative/1
                     ])
            ))
          ]);

rule(alternative) ->
  peg:choose([fun sequence/1, fun primary/1]);

rule(primary) ->
  peg:choose([
              peg:seq([fun prefix/1, fun atomic/1]),
              peg:seq([fun atomic/1, fun suffix/1]),
              fun atomic/1
             ]);

rule(sequence) ->
  peg:seq([
           peg:label(head, fun labeled_sequence_primary/1),
           peg:label(tail, peg:one_or_more(
             peg:seq([
                      fun space/1,
                      fun labeled_sequence_primary/1
                     ])
            ))
          ]);

rule(labeled_sequence_primary) -> peg:seq([peg:optional(fun label/1), fun primary/1]);

rule(label) -> peg:seq([fun alpha_char/1,
                        peg:zero_or_more(fun alphanumeric_char/1),
                        peg:string(":")
                       ]);

rule(suffix) ->
  peg:choose([fun repetition_suffix/1, fun optional_suffix/1]);

rule(optional_suffix) -> peg:string("?");

rule(repetition_suffix) -> peg:choose([peg:string("*"), peg:string("+")]);

rule(prefix) -> peg:choose([peg:string("&"), peg:string("!")]);

rule(atomic) ->
  peg:choose([fun terminal/1, fun nonterminal/1, fun parenthesized_expression/1]);

rule(parenthesized_expression) ->
  peg:seq([peg:string("("),
           peg:optional(fun space/1),
           fun parsing_expression/1,
           peg:optional(fun space/1),
           peg:string(")")
          ]);

rule(nonterminal) ->
  peg:seq([fun alpha_char/1,
           peg:zero_or_more(fun alphanumeric_char/1)]);

rule(terminal) ->
  peg:choose([fun quoted_string/1,
              fun character_class/1,
              fun anything_symbol/1]);

rule(quoted_string) -> peg:choose([fun single_quoted_string/1, fun double_quoted_string/1]);

rule(double_quoted_string) ->
  peg:seq([peg:string("\""),
           peg:label(string, peg:zero_or_more(
             peg:seq([peg:not_(peg:string("\"")),
                      peg:choose([peg:string("\\\\"), peg:string("\\\""), peg:anything()])]))),
           peg:string("\"")]);

rule(single_quoted_string) ->
  peg:seq([peg:string("'"),
           peg:label(string,peg:zero_or_more(
             peg:seq([peg:not_(peg:string("'")),
                      peg:choose([peg:string("\\\\"), peg:string("\\'"), peg:anything()])]))),
           peg:string("'")]);

rule(character_class) ->
  peg:seq([peg:string("["),
           peg:label(characters, peg:one_or_more(peg:seq([peg:not_(peg:string("]")),
                                    peg:choose([peg:seq([peg:string("\\\\"), peg:anything()]),
                                                peg:seq([peg:not_(peg:string("\\\\")), peg:anything()])])
                                   ]))),
           peg:string("]")]);

rule(anything_symbol) -> peg:string(".");

rule(non_space_char) -> peg:and_([peg:not_(fun space/1), peg:anything()]);

rule(alpha_char) -> peg:charclass("[A-Za-z_]");

rule(alphanumeric_char) -> peg:choose([fun alpha_char/1, peg:charclass("[0-9]")]);

rule(space) -> peg:one_or_more(peg:choose([fun white/1, fun comment_to_eol/1]));

rule(comment_to_eol) ->
  peg:seq([peg:string("%"),
           peg:zero_or_more(peg:and_([peg:not_(peg:string("\n")), peg:anything()]))]);

rule(white) -> peg:charclass("[ \t\n\r]").

transform(rules, Node) ->
  Rules = string:join(lists:nth(2, Node), ";\n"),
  Rules ++ ".\n";
transform(declaration_sequence, Node) ->
  [proplists:get_value(head, Node)|lists:map(fun(I) -> lists:nth(2, I) end, proplists:get_value(tail, Node, []))];
transform(declaration, Node) ->
  "rule("++element(2,hd(Node))++") ->\n  " ++ lists:nth(5, Node);
transform(sequence, Node) ->
  Tail = [lists:nth(2, S) || S <- proplists:get_value(tail, Node)],
  Statements = [proplists:get_value(head, Node)|Tail],
  "peg:seq(["++ string:join(Statements, ", ") ++ "])";
transform(choice, Node) ->
  Tail = [lists:last(S) || S <- proplists:get_value(tail, Node)],
  Statements = [proplists:get_value(head, Node)|Tail],
  "peg:choose([" ++ string:join(Statements, ", ") ++ "])";
transform(label, Node) ->
  lists:reverse(tl(lists:reverse(lists:flatten(Node))));
transform(labeled_sequence_primary, Node) ->
  case hd(Node) of
    [] -> lists:nth(2, Node);
    Label -> "peg:label('" ++ Label ++ "', "++lists:nth(2, Node)++")"
  end;
transform(single_quoted_string, Node) ->
  transform(double_quoted_string, Node);
transform(double_quoted_string, Node) ->
  "peg:string(\""++lists:flatten(proplists:get_value(string, Node))++"\")";
transform(character_class, Node) ->
  "peg:charclass(\"[" ++ lists:flatten(proplists:get_value(characters, Node)) ++ "\")";
transform(atomic, {nonterminal, Symbol}) ->
  "fun " ++ Symbol ++ "/1";
transform(primary, [Atomic, one_or_more]) ->
  "peg:one_or_more("++Atomic++")";
transform(primary, [Atomic, zero_or_more]) ->
  "peg:zero_or_more("++Atomic++")";
transform(primary, [Atomic, optional]) ->
  "peg:optional("++Atomic++")";
transform(primary, [assert, Atomic])->
  "peg:assert("++Atomic++")";
transform(primary, [not_, Atomic]) ->
  "peg:not_("++Atomic++")";
transform(nonterminal, Node) ->
  {nonterminal, lists:flatten(Node)};
transform(anything_symbol, _Node) ->
  "peg:anything()";
transform(suffix, Node) ->
  case Node of
    "*" -> zero_or_more;
    "+" -> one_or_more;
    "?" -> optional
  end;
transform(prefix, Node) ->
  case Node of
    "&" -> assert;
    "!" -> not_
  end;
transform(_, Node) ->
   Node.
