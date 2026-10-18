import gleam/io
import gleam/list
import gleam/string
import neotoma/ir/grammar as g
import neotoma/passes/concrete_erlang
import neotoma/passes/expand_charclasses
import neotoma/passes/expand_repetition
import neotoma/passes/memo_analysis
import neotoma/passes/prohibit_indirect_lr
import neotoma/passes/rewrite_simple_lr

// import neotoma/passes/expand_charclasses
import neotoma/passes/generate_abstract
import neotoma/syntax

pub fn main() -> Nil {
  let phase1 =
    g.Grammar(name: "generate_abstract_memoization_test", rules: [
      g.Definition(
        name: "start",
        expr: g.Sequence([
          g.Primary(g.Atomic(g.Terminal(g.String("(")))),
          g.Primary(g.OneOrMore(g.Primary(g.Atomic(g.Nonterminal("inner"))))),
          g.Primary(g.Atomic(g.Terminal(g.String(")")))),
        ]),
      ),
      g.Definition(
        name: "inner",
        expr: g.Primary(g.Atomic(g.Terminal(g.Anything))),
      ),
    ])
    |> expand_charclasses.expand_charclasses
    |> expand_repetition.expand_repetition
    |> rewrite_simple_lr.rewrite_simple_lr
    |> prohibit_indirect_lr.prohibit_indirect_lr

  case phase1 {
    Ok(g) ->
      g
      |> generate_abstract.generate_abstract_module(memo_analysis.memo_analysis(
        g,
      ))
      |> concrete_erlang.lower
      |> syntax.format
      |> io.println

    Error(prohibit_indirect_lr.CyclesDetected(cycles)) -> {
      let cycles =
        list.map(cycles, string.join(_, " -> ")) |> string.join("\n  ")
      io.print_error(
        "Indirect left-recursion detected in grammar:\n  " <> cycles <> "\n",
      )
    }
  }
  // TODO: optimization pass that promotes nested cases inside an ignore-branch, that match on the same subject
}
