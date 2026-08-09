import neotoma/syntax
import neotoma/passes/generate_wrapper
import neotoma/passes/functionize_rules
import gleam/io
import neotoma/grammar.{Grammar, Position, Definition, Span, Terminal}

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
  // let _ =
  //   Grammar([
  //     Definition(
  //       name: "g",
  //       expr: Terminal(
  //         kind: grammar.Anything,
  //         span: Span(start: Position(0, 0, 0), end: Position(15, 0, 15)),
  //       ),
  //     ),
  //   ])
  //   |> functionize_rules.functionize_rules
  //   |> generate_wrapper.generate_wrapper
  //   |> syntax.format
  //   |> io.println
  todo
}
