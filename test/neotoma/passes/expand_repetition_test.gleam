import birdie
import neotoma/grammar as g
import neotoma/passes/expand_repetition
import pprint

pub fn expand_repetition_simple_star_test() {
  let name = "expand_repetition_simple_star"
  let input =
    g.Grammar(name:, rules: [
      g.Definition(
        name: "root",
        expr: g.Primary(
          g.ZeroOrMore(g.Primary(g.Atomic(g.Terminal(g.String("neotoma"))))),
        ),
      ),
    ])

  input
  |> expand_repetition.expand_repetition
  |> pprint.format
  |> birdie.snap(name)
}

pub fn expand_repetition_simple_plus_test() {
  let name = "expand_repetition_simple_plus"
  let input =
    g.Grammar(name:, rules: [
      g.Definition(
        name: "root",
        expr: g.Primary(
          g.OneOrMore(g.Primary(g.Atomic(g.Terminal(g.String("neotoma"))))),
        ),
      ),
    ])

  input
  |> expand_repetition.expand_repetition
  |> pprint.format
  |> birdie.snap(name)
}

pub fn expand_repetition_sequence_star_test() {
  let name = "expand_repetition_sequence_star"
  let input =
    g.Grammar(name:, rules: [
      g.Definition(
        name: "root",
        expr: g.Primary(
          g.ZeroOrMore(
            g.Sequence([
              g.Primary(g.Atomic(g.Terminal(g.String("a")))),
              g.Primary(g.Atomic(g.Terminal(g.Anything))),
            ]),
          ),
        ),
      ),
    ])

  input
  |> expand_repetition.expand_repetition
  |> pprint.format
  |> birdie.snap(name)
}

pub fn expand_repetition_choice_star_test() {
  let name = "expand_repetition_simple_star"
  let input =
    g.Grammar(name:, rules: [
      g.Definition(
        name: "root",
        expr: g.Primary(
          g.ZeroOrMore(
            g.Choice([
              g.Primary(g.Atomic(g.Terminal(g.String("a")))),
              g.Primary(g.Atomic(g.Terminal(g.String("b")))),
            ]),
          ),
        ),
      ),
    ])
  input
  |> expand_repetition.expand_repetition
  |> pprint.format
  |> birdie.snap(name)
}

pub fn expand_repetition_charclass_star_test() {
  let name = "expand_repetition_charclass_star"
  let input =
    g.Grammar(name:, rules: [
      g.Definition(
        name: "root",
        expr: g.Primary(
          g.ZeroOrMore(
            g.Primary(
              g.Atomic(
                g.Terminal(
                  g.CharacterClass([
                    g.CharacterRange("a", "z"),
                    g.SingleCharacter("@"),
                  ]),
                ),
              ),
            ),
          ),
        ),
      ),
    ])

  input
  |> expand_repetition.expand_repetition
  |> pprint.format
  |> birdie.snap(name)
}

pub fn expand_repetition_dedupe_simple_test() {
  let name = "expand_repetition_dedupe_simple"
  let input =
    g.Grammar(name:, rules: [
      g.Definition(
        name: "root",
        expr: g.Sequence([
          g.Primary(
            g.ZeroOrMore(g.Primary(g.Atomic(g.Nonterminal("whitespace")))),
          ),
          g.Primary(g.Atomic(g.Terminal(g.String("neotoma")))),
          g.Primary(
            g.ZeroOrMore(g.Primary(g.Atomic(g.Nonterminal("whitespace")))),
          ),
        ]),
      ),
      g.Definition(
        name: "whitespace",
        expr: g.Choice([
          g.Primary(g.Atomic(g.Terminal(g.String(" ")))),
          g.Primary(g.Atomic(g.Terminal(g.String("\t")))),
          g.Primary(g.Atomic(g.Terminal(g.String("\r")))),
          g.Primary(g.Atomic(g.Terminal(g.String("\n")))),
        ]),
      ),
    ])

  input
  |> expand_repetition.expand_repetition
  |> pprint.format
  |> birdie.snap(name)
}

pub fn expand_repetition_dedupe_complex_test() {
  let name = "expand_repetition_dedupe_complex"
  let duped =
    g.Primary(
      g.ZeroOrMore(
        g.Sequence([
          g.Primary(g.Atomic(g.Terminal(g.String("a")))),
          g.Primary(g.Atomic(g.Terminal(g.Anything))),
        ]),
      ),
    )
  let input =
    g.Grammar(name:, rules: [
      g.Definition(
        name: "root",
        expr: g.Sequence([
          duped,
          g.Primary(g.Atomic(g.Terminal(g.String("neotoma")))),
          duped,
        ]),
      ),
    ])

  input
  |> expand_repetition.expand_repetition
  |> pprint.format
  |> birdie.snap(name)
}
