import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/pair
import gleam/result
import gleam/set
import neotoma/ir/norep as g

/// A measure of the cost of a particular non-terminal's definition
pub type Cost {
  /// The non-terminal has a discrete/non-recursive definition
  Discrete(Int)
  /// The non-terminal has potentially unbounded recursion
  Infinite
}

/// Produces a list of nonterminals in the grammar that should be memoized.
pub fn memo_analysis(grammar: g.Grammar) -> set.Set(String) {
  let all_nts =
    grammar.rules
    |> list.map(fn(definition) { definition.name })
    |> set.from_list()
  let virtual_inlined =
    grammar.rules
    |> find_virtual_inline_rules([])
    |> list.map(fn(definition) { definition.name })
    |> set.from_list()
  set.difference(all_nts, virtual_inlined)
}

fn find_virtual_inline_rules(
  definitions: List(g.Definition),
  vnts: List(g.Definition),
) -> List(g.Definition) {
  case find_smallest_vnt(vnts, definitions) {
    option.Some(nt) -> {
      let assert Ok(def) = find_definition(definitions, nt)
      find_virtual_inline_rules(definitions, [def, ..vnts])
    }
    option.None -> vnts
  }
}

fn find_smallest_vnt(
  vnts: List(g.Definition),
  definitions: List(g.Definition),
) -> option.Option(String) {
  definitions
  |> list.fold(option.None, fn(candidate, definition) {
    let old_cost =
      candidate |> option.map(pair.second) |> option.unwrap(Infinite)
    let new_cost = cost_of(vnts, [definition.name], definition.expr)
    case is_candidate(vnts, old_cost, new_cost, definition.name) {
      True -> option.Some(#(definition.name, new_cost))
      False -> candidate
    }
  })
  |> option.map(pair.first)
}

fn cost_of(
  vnts: List(g.Definition),
  ns: List(String),
  expr: g.Expression,
) -> Cost {
  case expr {
    g.Primary(g.Atomic(g.Nonterminal(name:))) ->
      case list.contains(ns, name) {
        True -> Infinite
        False -> {
          case find_definition(vnts, name) {
            Ok(def) -> cost_of(vnts, [def.name, ..ns], def.expr)
            Error(_) -> Discrete(1)
          }
        }
      }
    g.Primary(g.Atomic(g.Terminal(kind: g.Anything))) -> Discrete(1)
    g.Primary(g.Atomic(g.Terminal(kind: g.String(str: _)))) -> Discrete(1)
    g.Primary(g.Atomic(g.Terminal(kind: g.CharacterClass(chars: _)))) ->
      Discrete(1)
    // TODO: examine cost of character classes
    g.Primary(g.Atomic(g.Epsilon)) -> Discrete(0)
    g.Primary(g.Optional(expression))
    | g.Primary(g.Deny(expression))
    | g.Primary(g.Assert(expression)) -> cost_of(vnts, ns, expression)
    g.Choice(items) | g.Sequence(items) ->
      list.fold(items, Discrete(1), fn(sum, item) {
        add_cost(sum, cost_of(vnts, ns, item))
      })
  }
}

fn is_candidate(
  vnts: List(g.Definition),
  current: Cost,
  new: Cost,
  name: String,
) -> Bool {
  { compare_cost(new, current) == order.Lt }
  && { compare_cost(new, Discrete(25)) == order.Lt }
  && result.is_error(find_definition(vnts, name))
}

fn find_definition(
  definitions: List(g.Definition),
  name: String,
) -> Result(g.Definition, Nil) {
  list.find(definitions, fn(definition) { definition.name == name })
}

fn add_cost(a: Cost, b: Cost) -> Cost {
  case a, b {
    Discrete(a), Discrete(b) -> Discrete(a + b)
    _, _ -> Infinite
  }
}

fn compare_cost(a: Cost, b: Cost) -> order.Order {
  case a, b {
    Discrete(a), Discrete(b) -> int.compare(a, b)
    Discrete(_), Infinite -> order.Lt
    Infinite, Discrete(_) -> order.Gt
    Infinite, Infinite -> order.Eq
  }
}
