import birdie
import neotoma/abstract as a
import neotoma/ir/g_rec as g
import neotoma/passes/generate_abstract as gen
import pprint

pub fn generate_abstract_simple_sequence_test() {
  let seq = [g.Primary(g.Atomic(g.Terminal(kind: g.String("neotoma"))))]
  // let input_var = "Input"
  // let expected = [
  //   a.Case(a.Variable(input_var), [
  //     a.CaseClause(
  //       a.BinaryPattern([
  //         a.BinaryStringField("neotoma"),
  //         a.BinaryVariable("Remainder2", [a.Bytes]),
  //       ]),
  //       [
  //         a.Tuple([
  //           a.Atom("ok"),
  //           a.Tuple([
  //             a.Binary([a.BinaryStringField("neotoma")]),
  //             a.Variable("Remainder2"),
  //           ]),
  //         ]),
  //       ],
  //     ),
  //     a.CaseClause(a.Ignore, [a.Tuple([a.Atom("error"), a.Atom("no_match")])]),
  //   ]),
  // ]

  gen.generate_sequence(seq, ["Input"], default_success, default_failure)
  |> pprint.format
  |> birdie.snap(title: "sequence with one item")
}

pub fn generate_abstract_two_primary_sequence_test() {
  let seq = [
    g.Primary(g.Atomic(g.Terminal(kind: g.String("neotoma")))),
    g.Primary(g.Atomic(g.Terminal(kind: g.String("rocks")))),
  ]
  // let input_var = "Input"

  // let expected = [
  //   a.Case(a.Variable(input_var), [
  //     a.CaseClause(
  //       a.BinaryPattern([
  //         a.BinaryStringField("neotoma"),
  //         a.BinaryVariable("Remainder2", [a.Bytes]),
  //       ]),
  //       [
  //         a.Case(a.Variable("Remainder2"), [
  //           a.CaseClause(
  //             a.BinaryPattern([
  //               a.BinaryStringField("rocks"),
  //               a.BinaryVariable("Remainder3", [a.Bytes]),
  //             ]),
  //             [
  //               a.Tuple([
  //                 a.Atom("ok"),
  //                 a.Tuple([
  //                   a.Binary([a.BinaryStringField("rocks")]),
  //                   a.Variable("Remainder3"),
  //                 ]),
  //               ]),
  //             ],
  //           ),
  //           a.CaseClause(a.Ignore, [
  //             a.Tuple([a.Atom("error"), a.Atom("no_match")]),
  //           ]),
  //         ]),
  //       ],
  //     ),
  //     a.CaseClause(a.Ignore, [a.Tuple([a.Atom("error"), a.Atom("no_match")])]),
  //   ]),
  // ]

  gen.generate_sequence(seq, ["Input"], default_success, default_failure)
  |> pprint.format
  |> birdie.snap(title: "sequence with two items")
}

pub fn generate_abstract_simple_choice_test() {
  let choices = [
    g.Primary(g.Atomic(g.Terminal(kind: g.String("neotoma")))),
    //    g.Primary(g.Atomic(g.Terminal(kind: g.String("rocks"), span:)))
  ]
  // let input_var = "Input"
  // let expected = [
  //   a.Case(a.Variable(input_var), [
  //     a.CaseClause(
  //       a.BinaryPattern([
  //         a.BinaryStringField("neotoma"),
  //         a.BinaryVariable("Remainder2", [a.Bytes]),
  //       ]),
  //       [
  //         a.Tuple([
  //           a.Atom("ok"),
  //           a.Tuple([
  //             a.Binary([a.BinaryStringField("neotoma")]),
  //             a.Variable("Remainder2"),
  //           ]),
  //         ]),
  //       ],
  //     ),
  //     a.CaseClause(a.Ignore, [a.Tuple([a.Atom("error"), a.Atom("no_match")])]),
  //   ]),
  // ]

  gen.generate_choice(choices, ["Input"], default_success, default_failure)
  |> pprint.format
  |> birdie.snap(title: "choice with one item")
}

pub fn generate_abstract_two_item_choice_test() {
  let choices = [
    g.Primary(g.Atomic(g.Terminal(kind: g.String("neotoma")))),
    g.Primary(g.Atomic(g.Terminal(kind: g.String("rocks")))),
  ]
  // let input_var = "Input"
  // let expected = [
  //   a.Case(a.Variable(input_var), [
  //     a.CaseClause(
  //       a.BinaryPattern([
  //         a.BinaryStringField("neotoma"),
  //         a.BinaryVariable("Remainder2", [a.Bytes]),
  //       ]),
  //       [
  //         a.Tuple([
  //           a.Atom("ok"),
  //           a.Tuple([
  //             a.Binary([a.BinaryStringField("neotoma")]),
  //             a.Variable("Remainder2"),
  //           ]),
  //         ]),
  //       ],
  //     ),
  //     a.CaseClause(a.Ignore, [a.Tuple([a.Atom("error"), a.Atom("no_match")])]),
  //   ]),
  // ]

  gen.generate_choice(choices, ["Input"], default_success, default_failure)
  |> pprint.format
  |> birdie.snap(title: "choice with two items")
}

pub fn generate_abstract_nonterminal_test() {
  let g =
    g.Grammar(name: "generate_abstract_nonterminal_test", rules: [
      g.Definition(
        name: "start",
        expr: g.Sequence([
          g.Primary(g.Atomic(g.Terminal(g.String("(")))),
          g.Primary(g.Atomic(g.Nonterminal("inner"))),
          g.Primary(g.Atomic(g.Terminal(g.String(")")))),
        ]),
      ),
      g.Definition(
        name: "inner",
        expr: g.Primary(g.Atomic(g.Terminal(g.Anything))),
      ),
    ])
  gen.generate_abstract_module(g)
  |> pprint.format
  |> birdie.snap(title: "generate abstract nonterminal")
}

pub fn generate_abstract_assert_lookahead_test() {
  // (&"neo" ( "neotoma" / "neon" ) ) / ....
  let expr =
    g.Sequence([
      g.Primary(g.Assert(g.Primary(g.Atomic(g.Terminal(g.String("neo")))))),
      g.Choice([
        g.Primary(g.Atomic(g.Terminal(g.String("neotoma")))),
        g.Primary(g.Atomic(g.Terminal(g.String("neon")))),
      ]),
    ])

  gen.generate_expression(expr, ["Input"], default_success, default_failure)
  |> pprint.format
  |> birdie.snap(title: "generate abstract positive lookahead")
}

pub fn generate_abstract_deny_lookahead_test() {
  let expr =
    // (!crlf .)
    g.Sequence([
      g.Primary(g.Deny(g.Primary(g.Atomic(g.Nonterminal("crlf"))))),
      g.Primary(g.Atomic(g.Terminal(g.Anything))),
    ])

  gen.generate_expression(expr, ["Input"], default_success, default_failure)
  |> pprint.format
  |> birdie.snap(title: "generate abstract negative lookahead")
}

pub fn generate_abstract_optional_test() {
  let expr =
    g.Sequence([
      g.Primary(g.Optional(g.Primary(g.Atomic(g.Nonterminal("whitespace"))))),
      g.Primary(g.Atomic(g.Terminal(g.String("neotoma")))),
    ])

  gen.generate_expression(expr, ["Input"], default_success, default_failure)
  |> pprint.format
  |> birdie.snap(title: "generate abstract optional")
}

fn default_success(value, remainder) {
  [a.Tuple([a.Atom("ok"), a.Tuple([value, a.Variable(remainder)])])]
}

fn default_failure() {
  [a.Tuple([a.Atom("error"), a.Atom("no_match")])]
}
