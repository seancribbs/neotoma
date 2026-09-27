import gleam/list
import neotoma/ir/norep as g

pub fn simplify(g: g.Grammar) -> g.Grammar {
  fixpoint(g, simplify_grammar)
}

fn simplify_grammar(grammar: g.Grammar) -> g.Grammar {
  grammar
  |> flatten_grammar()
  // Peephole optimization 1: flatten redundant sequences and choices
  // collapse()
  // inline()
}

fn flatten_grammar(grammar: g.Grammar) -> g.Grammar {
  g.Grammar(..grammar, rules: list.map(grammar.rules, flatten_definition))
}

fn flatten_definition(definition: g.Definition) -> g.Definition {
  g.Definition(..definition, expr: flatten_expr(definition.expr))
}

fn flatten_expr(expr: g.Expression) -> g.Expression {
  case expr {
    // Eliminate useless sequencing
    g.Sequence([item]) -> flatten_expr(item)
    g.Sequence(items) ->
      items
      |> list.map(flatten_expr)
      |> list.flat_map(flatten_sequence) // Flatten sequence-in-sequence
      |> g.Sequence
    // Eliminate redundant choice
    g.Choice([alt]) -> flatten_expr(alt)
    g.Choice(alts) ->
      alts
      |> list.map(flatten_expr)
      |> list.flat_map(flatten_choice) // Flatten choice-in-choice
      |> g.Choice

    g.Primary(prim) -> g.Primary(flatten_primary(prim))
  }
}

// Flattens direct nesting of choice-in-choice to single
fn flatten_choice(expression: g.Expression) -> List(g.Expression) {
  case expression {
    g.Choice(choices) -> choices
    _ -> [expression]
  }
}

// Flattens direct nesting of sequence-in-sequence to single sequence
fn flatten_sequence(expression: g.Expression) -> List(g.Expression) {
  case expression {
    g.Sequence(items) -> items
    _ -> [expression]
  }
}

fn flatten_primary(prim: g.Primary) -> g.Primary {
  case prim {
    g.Assert(expr) -> g.Assert(flatten_expr(expr))
    g.Deny(expr) -> g.Deny(flatten_expr(expr))
    g.Optional(expr) -> g.Optional(flatten_expr(expr))
    _atomic -> prim
  }
}

/// Computes a fixpoint over a function recursively
fn fixpoint(value: a, xform: fn(a) -> a) -> a {
  case xform(value) {
    new if value == new -> new
    new -> fixpoint(new, xform)
  }
}
