import neotoma/abstract as a
import neotoma/grammar as g
import neotoma/passes/generate_abstract as gen

const span = g.Span(g.Position(0, 0, 0), g.Position(1, 1, 0))

pub fn generate_abstract_simple_sequence_test() {
  let seq = [g.Primary(g.Atomic(g.Terminal(kind: g.String("neotoma"), span:)))]
  let input_var = "Input"
  let expected = [
    a.Case(a.Variable(input_var), [
      a.CaseClause(
        a.BinaryPattern([
          a.BinaryStringField("neotoma"),
          a.BinaryVariable("Remainder2", [a.Bytes]),
        ]),
        [
          a.Tuple([
            a.Atom("ok"),
            a.Tuple([
              a.Binary([a.BinaryStringField("neotoma")]),
              a.Variable("Remainder2"),
            ]),
          ]),
        ],
      ),
      a.CaseClause(a.Ignore, [a.Tuple([a.Atom("error"), a.Atom("no_match")])]),
    ]),
  ]

  assert gen.generate_sequence(
      seq,
      ["Input"],
      fn(value, remainder) {
        [a.Tuple([a.Atom("ok"), a.Tuple([value, a.Variable(remainder)])])]
      },
      fn() { [a.Tuple([a.Atom("error"), a.Atom("no_match")])] },
    )
    == expected
}

pub fn generate_abstract_two_primary_sequence_test() {
  let seq = [
    g.Primary(g.Atomic(g.Terminal(kind: g.String("neotoma"), span:))),
    g.Primary(g.Atomic(g.Terminal(kind: g.String("rocks"), span:))),
  ]
  let input_var = "Input"

  let expected = [
    a.Case(a.Variable(input_var), [
      a.CaseClause(
        a.BinaryPattern([
          a.BinaryStringField("neotoma"),
          a.BinaryVariable("Remainder2", [a.Bytes]),
        ]),
        [
          a.Case(a.Variable("Remainder2"), [
            a.CaseClause(
              a.BinaryPattern([
                a.BinaryStringField("rocks"),
                a.BinaryVariable("Remainder3", [a.Bytes]),
              ]),
              [
                a.Tuple([
                  a.Atom("ok"),
                  a.Tuple([
                    a.Binary([a.BinaryStringField("rocks")]),
                    a.Variable("Remainder3"),
                  ]),
                ]),
              ],
            ),
            a.CaseClause(a.Ignore, [
              a.Tuple([a.Atom("error"), a.Atom("no_match")]),
            ]),
          ]),
        ],
      ),
      a.CaseClause(a.Ignore, [a.Tuple([a.Atom("error"), a.Atom("no_match")])]),
    ]),
  ]

  assert gen.generate_sequence(
      seq,
      ["Input"],
      fn(value, remainder) {
        [a.Tuple([a.Atom("ok"), a.Tuple([value, a.Variable(remainder)])])]
      },
      fn() { [a.Tuple([a.Atom("error"), a.Atom("no_match")])] },
    )
    == expected
}

pub fn generate_abstract_simple_choice_test() {
  let choices = [
    g.Primary(g.Atomic(g.Terminal(kind: g.String("neotoma"), span:))),
    //    g.Primary(g.Atomic(g.Terminal(kind: g.String("rocks"), span:)))
  ]
  let input_var = "Input"
  let expected = [
    a.Case(a.Variable(input_var), [
      a.CaseClause(
        a.BinaryPattern([
          a.BinaryStringField("neotoma"),
          a.BinaryVariable("Remainder2", [a.Bytes]),
        ]),
        [
          a.Tuple([
            a.Atom("ok"),
            a.Tuple([
              a.Binary([a.BinaryStringField("neotoma")]),
              a.Variable("Remainder2"),
            ]),
          ]),
        ],
      ),
      a.CaseClause(a.Ignore, [a.Tuple([a.Atom("error"), a.Atom("no_match")])]),
    ]),
  ]

  assert gen.generate_choice(
      choices,
      ["Input"],
      fn(value, remainder) {
        [a.Tuple([a.Atom("ok"), a.Tuple([value, a.Variable(remainder)])])]
      },
      fn() { [a.Tuple([a.Atom("error"), a.Atom("no_match")])] },
    )
    == expected
}
