-module(metagrammar).
-include("../include/peg.hrl").

?root(rules).

rule(rules) ->
  peg:seq([peg:optional(fun space/1),
           fun declaration_sequence/1,
           peg:optional(fun space/1)]);

rule(declaration_sequence) ->
  peg:seq([
          fun declaration/1,
          peg:zero_or_more(
            peg:seq([
                     fun space/1,
                     fun declaration/1]))]);

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
           fun alternative/1,
           peg:one_or_more(
             peg:seq([
                      fun space/1,
                      peg:string("/"),
                      fun space/1,
                      fun alternative/1
                     ])
            )
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
               fun labeled_sequence_primary/1,
               peg:one_or_more(
                 peg:seq([
                          fun space/1,
                          fun labeled_sequence_primary/1
                         ])
                )
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
               peg:zero_or_more(
                 peg:seq([peg:not_(peg:string("\"")),
                          peg:choose([peg:string("\\\\"), peg:string("\\\""), peg:anything()])])),
               peg:string("\"")]);

rule(single_quoted_string) ->
      peg:seq([peg:string("'"),
               peg:zero_or_more(
                 peg:seq([peg:not_(peg:string("'")),
                          peg:choose([peg:string("\\\\"), peg:string("\\'"), peg:anything()])])),
               peg:string("'")]);

rule(character_class) ->
      peg:seq([peg:string("["),
               peg:one_or_more(peg:seq([peg:not_(peg:string("]")),
                                        peg:choose([peg:seq([peg:string("\\\\"), peg:anything()]),
                                                    peg:seq([peg:not_("\\\\"), peg:anything()])])
                                       ])),
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