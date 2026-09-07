/// This pass transforms Kleene star (*, zero-or-more) and plus (+, one-or-more)
/// into right-recursive rules.
///
/// See thesis, 4.2.2 Rewriting Iterative Rules
import gleam/dict
import gleam/list
import gleam/pair
import gleam/string
import gleam/string_tree
import neotoma/ir/grammar as g
import neotoma/ir/norep

pub fn expand_repetition(input: g.Grammar) -> norep.Grammar {
  input.rules
  |> list.flat_map(expand_repetition_declaration)
  |> prune_duplicates()
  |> norep.Grammar(name: input.name, rules: _)
}

fn prune_duplicates(defs: List(norep.Definition)) -> List(norep.Definition) {
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
) -> List(norep.Definition) {
  let #(expr, expansions) = expand_repetition_expr(definition.expr)
  [norep.Definition(name: definition.name, expr:), ..expansions]
}

fn expand_repetition_expr(
  expr: g.Expression,
) -> #(norep.Expression, List(norep.Definition)) {
  case expr {
    g.Primary(primary) ->
      expand_repetition_primary(primary) |> pair.map_first(norep.Primary)
    g.Sequence(exprs) -> {
      let #(expansions, exprs) =
        list.map_fold(exprs, [], fn(expansions, expr) {
          let #(expr, expansions2) = expand_repetition_expr(expr)
          #(list.append(expansions2, expansions), expr)
        })
      #(norep.Sequence(exprs), expansions)
    }
    g.Choice(exprs) -> {
      let #(expansions, exprs) =
        list.map_fold(exprs, [], fn(expansions, expr) {
          let #(expr, expansions2) = expand_repetition_expr(expr)
          #(list.append(expansions2, expansions), expr)
        })
      #(norep.Choice(exprs), expansions)
    }
  }
}

fn expand_repetition_primary(
  primary: g.Primary,
) -> #(norep.Primary, List(norep.Definition)) {
  case primary {
    g.Atomic(atomic) -> #(norep.Atomic(translate_atomic(atomic)), [])
    g.Assert(expr) -> {
      let #(expr, expansions) = expand_repetition_expr(expr)
      #(norep.Assert(expr), expansions)
    }
    g.Deny(expr) -> {
      let #(expr, expansions) = expand_repetition_expr(expr)
      #(norep.Deny(expr), expansions)
    }
    g.Optional(expr) -> {
      let #(expr, expansions) = expand_repetition_expr(expr)
      #(norep.Optional(expr), expansions)
    }
    g.ZeroOrMore(expr) -> {
      let name = generate_rule_name(expr, "star")
      let #(expr, expansions) = expand_repetition_expr(expr)
      let star =
        norep.Definition(
          name:,
          expr: norep.Choice([
            // TODO: Attach inline code that produces the cons-list
            norep.Sequence([
              expr,
              norep.Primary(norep.Atomic(norep.Nonterminal(name))),
            ]),
            norep.Primary(norep.Atomic(norep.Epsilon)),
          ]),
        )
      #(norep.Atomic(norep.Nonterminal(name)), [star, ..expansions])
    }
    g.OneOrMore(expr) -> {
      let name = generate_rule_name(expr, "plus")
      let #(expr, expansions) = expand_repetition_expr(expr)
      let plus =
        norep.Definition(
          name:,
          expr: norep.Choice([
            // TODO: Attach inline code that produces the cons-list
            norep.Sequence([
              expr,
              norep.Primary(norep.Atomic(norep.Nonterminal(name))),
            ]),
            expr,
          ]),
        )
      #(norep.Atomic(norep.Nonterminal(name)), [plus, ..expansions])
    }
  }
}

fn translate_atomic(atomic: g.Atomic) -> norep.Atomic {
  case atomic {
    g.Nonterminal(name:) -> norep.Nonterminal(name:)
    g.Terminal(kind: g.Anything) -> norep.Terminal(norep.Anything)
    g.Terminal(kind: g.String(str:)) -> norep.Terminal(norep.String(str:))
    g.Terminal(kind: g.CharacterClass(chars:)) ->
      chars
      |> list.map(fn(c) {
        case c {
          g.SingleCharacter(char:) -> norep.SingleCharacter(char:)
          g.CharacterRange(start:, end:) -> norep.CharacterRange(start:, end:)
        }
      })
      |> norep.CharacterClass
      |> norep.Terminal
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
