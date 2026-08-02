import gleam/erlang/atom
import gleam/list
import neotoma/syntax

pub fn generate_wrapper(rules: List(#(String, syntax.SyntaxTree))) -> syntax.SyntaxTree {
  rules
  |> list.map(generate_function)
  |> syntax.form_list
}

fn generate_function(syntax_tree: #(String, syntax.SyntaxTree)) -> syntax.SyntaxTree {
  let name = syntax.atom(atom.create(syntax_tree.0))
  let clause = syntax.clause([syntax.variable("Input")], [], [syntax_tree.1])
  syntax.function(name, [clause])
}
