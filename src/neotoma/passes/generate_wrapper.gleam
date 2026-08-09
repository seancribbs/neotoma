import gleam/erlang/atom
import gleam/list
import neotoma/syntax

pub fn generate_wrapper(rules: List(#(String, syntax.SyntaxTree))) -> syntax.SyntaxTree {
  let assert [#(root_rule, _), ..] = rules
  let functions = list.map(rules, generate_function)

  generate_module_attrs("g")
  |> list.append([generate_string_entrypoint(root_rule), ..functions])
  |> syntax.form_list
}

fn generate_function(syntax_tree: #(String, syntax.SyntaxTree)) -> syntax.SyntaxTree {
  let name = syntax.atom(atom.create(syntax_tree.0))
  let clause = syntax.clause([syntax.variable("Input")], [], [syntax_tree.1])
  syntax.function(name, [clause])
}

fn generate_module_attrs(module_name: String) -> List(syntax.SyntaxTree) {
  let modname = syntax.atom(atom.create(module_name))
  let module = syntax.atom(atom.create("module"))
  let export = syntax.atom(atom.create("export"))
  let string = syntax.atom(atom.create("string"))

  [
    syntax.attribute(module, [modname]),
    syntax.attribute(export, [syntax.list([syntax.arity_qualifier(string, syntax.abstract(1))])]),
  ]
}

fn generate_string_entrypoint(root: String) -> syntax.SyntaxTree {
  let root_rule = syntax.atom(atom.create(root))
  let name = syntax.atom(atom.create("string"))
  let input = syntax.variable("Input")
  let clause = syntax.clause([input], [], [
    syntax.local_application(root_rule, [input])
  ])
  syntax.function(name, [clause])
}
