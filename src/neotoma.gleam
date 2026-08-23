import gleam/io
import neotoma/grammar.{Definition, Grammar, Position, Span, Terminal, Primary,Atomic}
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
    Grammar(name: "g", rules: [
      Definition(
        name: "g",
        expr: Primary(Atomic(Terminal(
          kind: grammar.Anything,
          span: Span(start: Position(0, 0, 0), end: Position(15, 0, 15)),
        ))),
      ),
    ])
    |> expand_charclasses.expand_charclasses
    |> generate_abstract.generate_abstract_module
    |> concrete_erlang.lower
    |> syntax.format
    |> io.println
}
