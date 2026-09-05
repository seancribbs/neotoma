/// This pass transforms Kleene star (*, zero-or-more) and plus (+, one-or-more)
/// into right-recursive rules.
///
/// See thesis, 4.2.2 Rewriting Iterative Rules
import gleam/dict
import gleam/list
import gleam/pair
import gleam/string
import gleam/string_tree
import neotoma/grammar as g
import neotoma/ir/g_rec

pub fn expand_repetition(input: g.Grammar) -> g_rec.Grammar {
  input.rules
  |> list.flat_map(expand_repetition_declaration)
  |> prune_duplicates()
  |> g_rec.Grammar(name: input.name, rules: _)
}

fn prune_duplicates(defs: List(g_rec.Definition)) -> List(g_rec.Definition) {
  let assert [top, ..] = defs
  let pruned =
    list.fold(defs, dict.new(), fn(acc, def) {
      case dict.get(acc, def.name) {
        Error(_) -> dict.insert(acc, def.name, def)
        Ok(def2) if def2 == def -> acc
        Ok(other) ->
          panic as {
            "unequal definitions with the same name: "
            <> string.inspect(def)
            <> " != "
            <> string.inspect(other)
          }
      }
    })
    |> dict.delete(top.name)
    |> dict.values
  [top, ..pruned]
}

fn expand_repetition_declaration(
  definition: g.Definition,
) -> List(g_rec.Definition) {
  let #(expr, expansions) = expand_repetition_expr(definition.expr)
  [g_rec.Definition(name: definition.name, expr:), ..expansions]
}

fn expand_repetition_expr(
  expr: g.Expression,
) -> #(g_rec.Expression, List(g_rec.Definition)) {
  case expr {
    g.Primary(primary) ->
      expand_repetition_primary(primary) |> pair.map_first(g_rec.Primary)
    g.Sequence(exprs) -> {
      let #(expansions, exprs) =
        list.map_fold(exprs, [], fn(expansions, expr) {
          let #(expr, expansions2) = expand_repetition_expr(expr)
          #(list.append(expansions2, expansions), expr)
        })
      #(g_rec.Sequence(exprs), expansions)
    }
    g.Choice(exprs) -> {
      let #(expansions, exprs) =
        list.map_fold(exprs, [], fn(expansions, expr) {
          let #(expr, expansions2) = expand_repetition_expr(expr)
          #(list.append(expansions2, expansions), expr)
        })
      #(g_rec.Choice(exprs), expansions)
    }
  }
}

fn expand_repetition_primary(
  primary: g.Primary,
) -> #(g_rec.Primary, List(g_rec.Definition)) {
  case primary {
    g.Atomic(atomic) -> #(g_rec.Atomic(translate_atomic(atomic)), [])
    g.Assert(expr) -> {
      let #(expr, expansions) = expand_repetition_expr(expr)
      #(g_rec.Assert(expr), expansions)
    }
    g.Deny(expr) -> {
      let #(expr, expansions) = expand_repetition_expr(expr)
      #(g_rec.Deny(expr), expansions)
    }
    g.Optional(expr) -> {
      let #(expr, expansions) = expand_repetition_expr(expr)
      #(g_rec.Optional(expr), expansions)
    }
    g.ZeroOrMore(expr) -> {
      let name = generate_rule_name(expr, "star")
      let #(expr, expansions) = expand_repetition_expr(expr)
      let star =
        g_rec.Definition(
          name:,
          expr: g_rec.Choice([
            // TODO: Attach inline code that produces the cons-list
            g_rec.Sequence([
              expr,
              g_rec.Primary(g_rec.Atomic(g_rec.Nonterminal(name))),
            ]),
            g_rec.Primary(g_rec.Atomic(g_rec.Epsilon)),
          ]),
        )
      #(g_rec.Atomic(g_rec.Nonterminal(name)), [star, ..expansions])
    }
    g.OneOrMore(expr) -> {
      let name = generate_rule_name(expr, "plus")
      let #(expr, expansions) = expand_repetition_expr(expr)
      let plus =
        g_rec.Definition(
          name:,
          expr: g_rec.Choice([
            // TODO: Attach inline code that produces the cons-list
            g_rec.Sequence([
              expr,
              g_rec.Primary(g_rec.Atomic(g_rec.Nonterminal(name))),
            ]),
            expr,
          ]),
        )
      #(g_rec.Atomic(g_rec.Nonterminal(name)), [plus, ..expansions])
    }
  }
}

fn translate_atomic(atomic: g.Atomic) -> g_rec.Atomic {
  case atomic {
    g.Nonterminal(name:) -> g_rec.Nonterminal(name:)
    g.Terminal(kind: g.Anything) -> g_rec.Terminal(g_rec.Anything)
    g.Terminal(kind: g.String(str:)) -> g_rec.Terminal(g_rec.String(str:))
    g.Terminal(kind: g.CharacterClass(chars:)) ->
      chars
      |> list.map(fn(c) {
        case c {
          g.SingleCharacter(char:) -> g_rec.SingleCharacter(char:)
          g.CharacterRange(start:, end:) -> g_rec.CharacterRange(start:, end:)
        }
      })
      |> g_rec.CharacterClass
      |> g_rec.Terminal
  }
}

fn generate_rule_name(expr: g.Expression, suffix: String) -> String {
  generate_rule_name_expr(expr)
  |> string_tree.append("_")
  |> string_tree.append(suffix)
  |> string_tree.to_string()
}

fn generate_rule_name_expr(expr: g.Expression) -> string_tree.StringTree {
  case expr {
    g.Primary(prim) -> generate_rule_name_primary(prim)
    g.Sequence(exprs) ->
      exprs
      |> list.map(generate_rule_name_expr)
      |> string_tree.join("_")
      |> string_tree.prepend("seq_")
    g.Choice(exprs) ->
      exprs
      |> list.map(generate_rule_name_expr)
      |> string_tree.join("_")
      |> string_tree.prepend("choice_")
  }
}

fn generate_rule_name_primary(prim: g.Primary) -> string_tree.StringTree {
  case prim {
    g.Atomic(atom) -> generate_rule_name_atomic(atom)
    g.Assert(expr) ->
      generate_rule_name_expr(expr) |> string_tree.append("_assert")
    g.Deny(expr) -> generate_rule_name_expr(expr) |> string_tree.append("_deny")
    g.Optional(expr) ->
      generate_rule_name_expr(expr) |> string_tree.append("_opt")
    g.ZeroOrMore(expr) ->
      generate_rule_name_expr(expr) |> string_tree.append("_star")
    g.OneOrMore(expr) ->
      generate_rule_name_expr(expr) |> string_tree.append("_plus")
  }
}

fn generate_rule_name_atomic(atom: g.Atomic) -> string_tree.StringTree {
  case atom {
    g.Nonterminal(name:) -> string_tree.from_string(name)
    g.Terminal(kind: g.Anything) -> string_tree.from_string("dot")
    g.Terminal(kind: g.String(str:)) -> string_tree.from_string(str)
    g.Terminal(kind: g.CharacterClass(chars:)) -> {
      list.map(chars, fn(c) {
        case c {
          g.SingleCharacter(char:) -> string_tree.from_string(char)
          g.CharacterRange(start:, end:) ->
            string_tree.from_strings([start, "_to_", end])
        }
      })
      |> string_tree.concat()
    }
  }
}
