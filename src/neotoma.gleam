import gleam/io
import neotoma/grammar as g
import neotoma/passes/concrete_erlang
import neotoma/passes/expand_charclasses
import neotoma/passes/generate_abstract
import neotoma/syntax

// functionize_rules
//   Converts abstract rules to functions - continuation passing style?
//   - Arguments:
//     1. Grammar (intermediate form)
//   - Output:
//     list of rule/syntax tree pairs, code block from user, set of utility function names
//
// generate_wrapper
//   Generates an erlang module from the inputs
//   - Arguments:
//       1. list of rules with their syntax tree bodies
//       2. non-rule auxillary code supplied by the user
//       3. utility functions used
//   - Output: whole erlang module, as a syntax tree

pub fn main() -> Nil {
  let _ =
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
    |> expand_charclasses.expand_charclasses
    |> generate_abstract.generate_abstract_module
    |> concrete_erlang.lower
    |> syntax.format
    |> io.println
}
