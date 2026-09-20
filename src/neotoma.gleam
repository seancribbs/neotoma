import gleam/list
import gleam/io
import gleam/string
import neotoma/ir/grammar as g
import neotoma/passes/concrete_erlang
import neotoma/passes/expand_charclasses
import neotoma/passes/expand_repetition
import neotoma/passes/prohibit_indirect_lr
import neotoma/passes/rewrite_simple_lr

// import neotoma/passes/expand_charclasses
import neotoma/passes/generate_abstract
import neotoma/syntax

pub fn main() -> Nil {
  let phase1 =
    g.Grammar("rewrite_simple_ir_simple_lr", [
      g.Definition(
        name: "expression",
        expr: g.Choice([
          g.Sequence([
            g.Primary(g.Atomic(g.Nonterminal("expression"))),
            g.Primary(g.Atomic(g.Terminal(g.String("+")))),
            g.Primary(g.Atomic(g.Nonterminal("number"))),
          ]),
          g.Sequence([
            g.Primary(g.Atomic(g.Nonterminal("subexpression"))),
            g.Primary(g.Atomic(g.Terminal(g.String("-")))),
            g.Primary(g.Atomic(g.Nonterminal("number"))),
          ]),
          g.Primary(g.Atomic(g.Nonterminal("number"))),
        ]),
      ),
      g.Definition(
        name: "subexpression",
        expr: g.Primary(g.Atomic(g.Nonterminal("expression")))
      ),
      g.Definition(
        name: "number",
        expr: g.Primary(
          g.Atomic(g.Terminal(g.CharacterClass([g.CharacterRange("0", "9")]))),
        ),
      ),
    ])
    |> expand_charclasses.expand_charclasses
    |> expand_repetition.expand_repetition
    |> rewrite_simple_lr.rewrite_simple_lr
    |> prohibit_indirect_lr.prohibit_indirect_lr

  case phase1 {
    Ok(g) ->
      g
      |> generate_abstract.generate_abstract_module
      |> concrete_erlang.lower
      |> syntax.format
      |> io.println

    Error(prohibit_indirect_lr.CyclesDetected(cycles)) -> {
      let cycles = list.map(cycles, string.join(_, " -> ")) |> string.join("\n  ")
      io.print_error(
        "Indirect left-recursion detected in grammar:\n  " <> cycles <> "\n",
      )
    }
  }
  // TODO: optimization pass that promotes nested cases inside an ignore-branch, that match on the same subject
}
