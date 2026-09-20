import gleam/list
import gleam/set
import neotoma/ir/norep as g

pub type CyclesDetected {
  CyclesDetected(cycles: List(List(String)))
}

type Cycle {
  Cycle(ordered: List(String))
}

pub fn prohibit_indirect_lr(g: g.Grammar) -> Result(g.Grammar, CyclesDetected) {
  let #(cycles, _) =
    list.fold(g.rules, #([], set.new()), fn(acc, definition) {
      case find_cycles(definition, g) {
        [] -> acc
        cycles ->
          list.fold(cycles, acc, fn(acc, cycle) {
            let #(ordered, cliques) = acc
            let members = set.from_list(cycle.ordered)
            case set.contains(cliques, members) {
              True -> acc
              False -> #([cycle.ordered, ..ordered], set.insert(cliques, members))
            }
          })
      }
    })
  case list.reverse(cycles) {
    [] -> Ok(g)
    cycles -> Error(CyclesDetected(cycles))
  }
}

fn find_cycles(definition: g.Definition, grammar: g.Grammar) -> List(Cycle) {
  find_cycle_step(definition, [], grammar)
}

fn find_cycle_step(
  definition: g.Definition,
  nts: List(String),
  grammar: g.Grammar,
) -> List(Cycle) {
  find_cycle_expr(definition.expr, [definition.name, ..nts], grammar)
}

fn find_cycle_primary(prim: g.Primary, nts: List(String), grammar: g.Grammar) {
  case prim {
    g.Atomic(g.Nonterminal(name)) -> {
      case list.contains(nts, name) {
        True -> {
          [Cycle(ordered: list.reverse([name, ..nts]))]
        }
        False -> {
          let assert Ok(def) =
            list.find(grammar.rules, fn(d) { d.name == name })
          find_cycle_step(def, nts, grammar)
        }
      }
    }
    g.Assert(expr) | g.Deny(expr) | g.Optional(expr) ->
      find_cycle_expr(expr, nts, grammar)
    _ -> []
  }
}

fn find_cycle_expr(
  expr: g.Expression,
  nts: List(String),
  grammar: g.Grammar,
) -> List(Cycle) {
  case expr {
    g.Primary(primary) -> find_cycle_primary(primary, nts, grammar)
    g.Sequence([expr, ..]) -> find_cycle_expr(expr, nts, grammar)
    g.Choice(choices) ->
      list.flat_map(choices, find_cycle_expr(_, nts, grammar))
    _ -> []
  }
}
