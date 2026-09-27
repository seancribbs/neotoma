import gleam/list
import neotoma/ir/norep as g

pub fn simplify(g: g.Grammar) -> g.Grammar {
  fixpoint(g, simplify_grammar)
}

fn simplify_grammar(grammar: g.Grammar) -> g.Grammar {
  grammar
  // Peephole optimization 1: flatten redundant sequences and choices
  |> flatten_grammar()
  // collapse()
  |> inline()
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
      |> list.flat_map(flatten_sequence)
      // Flatten sequence-in-sequence
      |> g.Sequence
    // Eliminate redundant choice
    g.Choice([alt]) -> flatten_expr(alt)
    g.Choice(alts) ->
      alts
      |> list.map(flatten_expr)
      |> list.flat_map(flatten_choice)
      // Flatten choice-in-choice
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

/// Inline all nonterminals that are only referenced once in a grammar. Also
/// eliminates any nonterminals not referenced anywhere except perhaps in their
/// own rule.
fn inline(grammar: g.Grammar) -> g.Grammar {
  let assert [_top, ..rest] = grammar.rules
  list.fold(rest, grammar, fn(grammar, definition) {
    let references = number_of_references(grammar.rules, definition.name)
    let self_references = references_in_expr(definition.expr, definition.name)
    let size_of_rule = g.size(definition.expr)

    case references, self_references, size_of_rule {
      0, 0, _ -> {
        // Not referenced by anything, including itself. We can simply remove
        // it.
        let rules = delete_rule(grammar.rules, definition.name)
        g.Grammar(..grammar, rules:)
      }
      refs, 0, size if { refs == 1 } || { refs > 1 && size <= 2 } -> {
        // Meets qualifications of inlining: referred to only once or is
        // sufficiently small and not self-recursive.
        let rules =
          grammar.rules
          |> list.map(inline_nonterminal(_, definition))
          |> delete_rule(definition.name)
        g.Grammar(..grammar, rules:)
      }
      _, _, _ -> {
        // Do not inline if any:
        // 1) referenced more than once and size of the rule is too big
        // 2) self-recursive
        grammar
      }
    }
  })
}

fn references_in_expr(expr: g.Expression, nonterminal: String) -> Int {
  case expr {
    g.Primary(g.Atomic(g.Nonterminal(name:))) if name == nonterminal -> 1
    g.Primary(g.Atomic(_)) -> 0
    g.Primary(g.Assert(expression)) ->
      references_in_expr(expression, nonterminal)
    g.Primary(g.Deny(expression)) -> references_in_expr(expression, nonterminal)
    g.Primary(g.Optional(expression)) ->
      references_in_expr(expression, nonterminal)
    g.Sequence(items) ->
      list.fold(items, 0, fn(acc, item) {
        acc + references_in_expr(item, nonterminal)
      })
    g.Choice(alts) ->
      list.fold(alts, 0, fn(acc, item) {
        acc + references_in_expr(item, nonterminal)
      })
  }
}

fn inline_nonterminal(
  definition: g.Definition,
  to_inline: g.Definition,
) -> g.Definition {
  let expr = inline_nonterminal_into_expression(definition.expr, to_inline)
  g.Definition(..definition, expr:)
}

fn inline_nonterminal_into_expression(
  expr: g.Expression,
  to_inline: g.Definition,
) -> g.Expression {
  case expr {
    g.Primary(g.Atomic(g.Nonterminal(name:))) if name == to_inline.name ->
      to_inline.expr
    g.Primary(g.Atomic(_)) -> expr
    g.Primary(g.Assert(expression)) ->
      g.Primary(
        g.Assert(inline_nonterminal_into_expression(expression, to_inline)),
      )
    g.Primary(g.Deny(expression)) ->
      g.Primary(
        g.Deny(inline_nonterminal_into_expression(expression, to_inline)),
      )
    g.Primary(g.Optional(expression)) ->
      g.Primary(
        g.Optional(inline_nonterminal_into_expression(expression, to_inline)),
      )
    g.Sequence(items) ->
      g.Sequence(
        list.map(items, inline_nonterminal_into_expression(_, to_inline)),
      )
    g.Choice(items) ->
      g.Choice(
        list.map(items, inline_nonterminal_into_expression(_, to_inline)),
      )
  }
}

fn delete_rule(rules: List(g.Definition), name: String) -> List(g.Definition) {
  list.fold_right(rules, [], fn(acc, rule) {
    case rule.name == name {
      True -> acc
      False -> [rule, ..acc]
    }
  })
}

fn number_of_references(rules: List(g.Definition), name: String) -> Int {
  list.fold(rules, 0, fn(acc, rule) {
    acc + references_in_expr(rule.expr, name)
  })
}

/// Computes a fixpoint over a function recursively
fn fixpoint(value: a, xform: fn(a) -> a) -> a {
  case xform(value) {
    new if value == new -> new
    new -> fixpoint(new, xform)
  }
}
