import birdie
import neotoma/ir/norep as g
import neotoma/passes/rewrite_simple_lr
import pprint

pub fn rewrite_simple_ir_no_recursion_test() {
  let title = "rewrite_simple_ir_no_recursion"
  let input =
    g.Grammar(title, [
      g.Definition("root", g.Primary(g.Atomic(g.Nonterminal("neotoma_star")))),
      g.Definition(
        "neotoma_star",
        g.Choice([
          g.Sequence([
            g.Primary(g.Atomic(g.Terminal(g.String("neotoma")))),
            g.Primary(g.Atomic(g.Nonterminal("neotoma_star"))),
          ]),
          g.Primary(g.Atomic(g.Epsilon)),
        ]),
      ),
    ])

  input
  |> rewrite_simple_lr.rewrite_simple_lr()
  |> pprint.format()
  |> birdie.snap(title:)
}

pub fn rewrite_simple_ir_simple_lr_test() {
  let title = "rewrite_simple_ir_simple_lr"
  let input =
    g.Grammar(title, [
      g.Definition(
        name: "expression",
        expr: g.Choice([
          g.Sequence([
            g.Primary(g.Atomic(g.Nonterminal("expression"))),
            g.Primary(g.Atomic(g.Terminal(g.String("+")))),
            g.Primary(g.Atomic(g.Nonterminal("number"))),
          ]),
          g.Sequence([
            g.Primary(g.Atomic(g.Nonterminal("expression"))),
            g.Primary(g.Atomic(g.Terminal(g.String("-")))),
            g.Primary(g.Atomic(g.Nonterminal("number"))),
          ]),
          g.Primary(g.Atomic(g.Nonterminal("number"))),
        ]),
      ),
      g.Definition(
        name: "number",
        expr: g.Primary(
          g.Atomic(g.Terminal(g.CharacterClass([g.CharacterRange("0", "9")]))),
        ),
      ),
    ])

  input
  |> rewrite_simple_lr.rewrite_simple_lr()
  |> pprint.format()
  |> birdie.snap(title:)
}

pub fn rewrite_simple_ir_indirect_lr_test() {
  // example from https://web.cs.ucla.edu/~todd/research/pepm08.pdf
  // x ::= <expr>
  // expr ::= <x> "-" <num> / <num>
  let title = "rewrite_simple_ir_indirect_lr"
  let input =
    g.Grammar(title, [
      g.Definition(name: "x", expr: g.Primary(g.Atomic(g.Nonterminal("expr")))),
      g.Definition(
        name: "expr",
        expr: g.Choice([
          g.Sequence([
            g.Primary(g.Atomic(g.Nonterminal("x"))),
            g.Primary(g.Atomic(g.Terminal(g.String("-")))),
            g.Primary(g.Atomic(g.Nonterminal("number"))),
          ]),
          g.Primary(g.Atomic(g.Nonterminal("number"))),
        ]),
      ),
      g.Definition(
        name: "number",
        expr: g.Primary(
          g.Atomic(g.Terminal(g.CharacterClass([g.CharacterRange("0", "9")]))),
        ),
      ),
    ])

  input
  |> rewrite_simple_lr.rewrite_simple_lr()
  |> pprint.format()
  |> birdie.snap(title:)
}
