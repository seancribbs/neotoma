import neotoma/code
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import neotoma/ir/grammar as g
import neotoma/passes/concrete_erlang
import neotoma/passes/expand_charclasses
import neotoma/passes/expand_repetition
import neotoma/passes/memo_analysis
import neotoma/passes/process_code_blocks
import neotoma/passes/prohibit_indirect_lr
import neotoma/passes/rewrite_simple_lr
import neotoma/passes/simplify

// import neotoma/passes/expand_charclasses
import neotoma/passes/generate_abstract
import neotoma/syntax

pub fn main() -> Nil {
  let phase1 =
    g.Grammar(
      name: "generate_abstract_memoization_test",
      rules: [
        g.Definition(
          name: "start",
          expr: g.Sequence(
            [
              g.Primary(g.Atomic(g.Terminal(g.String("(")))),
              g.Primary(
                g.Labeled(g.Primary(g.OneOrMore(g.Primary(g.Atomic(g.Nonterminal("inner"))))), "Inner"),
              ),
              g.Primary(g.Atomic(g.Terminal(g.String(")")))),
            ],
            option.Some(code.Unparsed("Inner")),
          ),
        ),
        g.Definition(
          name: "inner",
          expr: g.Primary(g.Atomic(g.Terminal(g.Anything))),
        ),
      ],
      top_level_code: option.None,
    )
    |> expand_charclasses.expand_charclasses
    |> expand_repetition.expand_repetition
    |> rewrite_simple_lr.rewrite_simple_lr
    |> prohibit_indirect_lr.prohibit_indirect_lr

  let handle_cycles = fn(e) {
    let prohibit_indirect_lr.CyclesDetected(cycles) = e
    let cycles = list.map(cycles, string.join(_, " -> ")) |> string.join("\n  ")
    io.print_error(
      "Indirect left-recursion detected in grammar:\n  " <> cycles <> "\n",
    )
    Nil
  }

  let _ = {
    use g1 <- result.try(result.map_error(phase1, handle_cycles))
    let g = simplify.simplify(g1)
    let memos = memo_analysis.memo_analysis(g)

    use phase2 <- result.map(process_code_blocks.process_code_blocks(g), _)

    phase2
    |> generate_abstract.generate_abstract_module(memos)
    |> concrete_erlang.lower
    |> syntax.format
    |> io.println
  }

  Nil
}
