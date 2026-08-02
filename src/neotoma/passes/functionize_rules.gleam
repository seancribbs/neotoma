import gleam/erlang/atom
import gleam/list
import neotoma/grammar as g
import neotoma/syntax

pub fn functionize_rules(
  grammar: g.Grammar,
) -> List(#(String, syntax.SyntaxTree)) {
  list.map(grammar.rules, functionize_rule)
}

fn functionize_rule(rule: g.Rule) -> #(String, syntax.SyntaxTree) {
  let tree = case rule.expr {
    g.Terminal(kind:, span: _) -> match_terminal(kind)
  }
  #(rule.name, tree)
}

fn match_terminal(kind: g.TerminalKind) -> syntax.SyntaxTree {
  case kind {
    g.String(str:) -> match_literal_string(str)
  }
}

fn match_literal_string(str: String) -> syntax.SyntaxTree {
  let failure_clause = syntax.clause([syntax.underscore()], [], [fail()])
  let success_clause = {
    let literal_string = syntax.string(str)
    let literal = syntax.binary([syntax.binary_field(literal_string)])
    let rest = syntax.variable("Rest")
    let pattern =
      syntax.binary([
        syntax.binary_field(literal_string),
        syntax.binary_field_with_types(rest, [
          syntax.atom(atom.create("binary")),
        ]),
      ])
    syntax.clause([pattern], [], [success(syntax.tuple([literal, rest]))])
  }

  syntax.case_expr(syntax.variable("Input"), [success_clause, failure_clause])
}

fn success(result: syntax.SyntaxTree) -> syntax.SyntaxTree {
  let ok = syntax.atom(atom.create("ok"))
  syntax.tuple([ok, result])
}

fn fail() -> syntax.SyntaxTree {
  let error = syntax.atom(atom.create("error"))
  let no_match = syntax.atom(atom.create("no_match"))
  syntax.tuple([error, no_match])
}
