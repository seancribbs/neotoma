import gleam/set
import neotoma/ir/norep as g
import neotoma/passes/memo_analysis

pub fn memo_analysis_chooses_self_recursive_rules_test() {
  let title = "memo_analysis_chooses_self_recursive_rules"
  let input =
  // expression <- number expression_tail
  // expression_tail <- "+" number expression_tail / "-" number expression_tail / EPSILON
  // number <- [0-9]
    g.Grammar(title, [
      g.Definition(
        name: "expression",
        expr: g.Sequence([
          g.Primary(g.Atomic(g.Nonterminal("number"))),
          g.Primary(g.Atomic(g.Nonterminal("expression_tail"))),
        ]),
      ),
      g.Definition(
        name: "expression_tail",
        expr: g.Choice([
          g.Sequence([
            g.Primary(g.Atomic(g.Terminal(g.String("+")))),
            g.Primary(g.Atomic(g.Nonterminal("number"))),
            g.Primary(g.Atomic(g.Nonterminal("expression_tail"))),
          ]),
          g.Sequence([
            g.Primary(g.Atomic(g.Terminal(g.String("-")))),
            g.Primary(g.Atomic(g.Nonterminal("number"))),
            g.Primary(g.Atomic(g.Nonterminal("expression_tail"))),
          ]),
          g.Primary(g.Atomic(g.Epsilon)),
        ]),
      ),
      g.Definition(
        name: "number",
        expr: g.Primary(
          g.Atomic(g.Terminal(g.CharacterClass([g.CharacterRange("0", "9")]))),
        ),
      ),
    ])

    assert set.from_list(["expression_tail"]) == memo_analysis.memo_analysis(input)
}

pub fn memo_analysis_chooses_mutually_recursive_rules_test() {
  todo
}

pub fn memo_analysis_excludes_simple_rules_test() {
  todo
}
