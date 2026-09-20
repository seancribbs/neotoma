import gleam/list
import neotoma/ir/norep as g

pub fn rewrite_simple_lr(g: g.Grammar) -> g.Grammar {
  let rules = list.flat_map(g.rules, rewrite_simple_lr_rule)
  g.Grammar(..g, rules:)
}

fn rewrite_simple_lr_rule(definition: g.Definition) -> List(g.Definition) {
  case definition.expr {
    g.Choice(choices) -> {
      case extract_lr(choices, definition.name) {
        // There were no directly-left-recursive choices, so no transformation
        // is necessary.
        #([], _) -> [definition]
        #(lrs, non_lrs) -> {
          // construct the tail rule
          let tail_name = definition.name <> "_tail"
          let tail =
            g.Definition(
              name: tail_name,
              expr: g.Choice(list.append(lrs, [g.Primary(g.Atomic(g.Epsilon))])),
            )
          // construct the replacement rule
          let definition =
            g.Definition(
              ..definition,
              expr: g.Choice(
                list.map(non_lrs, fn(expr) {
                  g.Sequence([
                    expr,
                    g.Primary(g.Atomic(g.Nonterminal(tail_name))),
                  ])
                }),
              ),
            )
          // Return the transformed rule
          [definition, tail]
        }
      }
    }
    g.Primary(_) | g.Sequence(_) -> [definition]
  }
}

fn extract_lr(
  choices: List(g.Expression),
  name: String,
) -> #(List(g.Expression), List(g.Expression)) {
  let tail_name = name <> "_tail"
  let tail_call = g.Primary(g.Atomic(g.Nonterminal(tail_name)))
  let expected = g.Primary(g.Atomic(g.Nonterminal(name)))
  use acc, choice <- list.fold_right(choices, #([], []))
  let #(lrs, non_lrs) = acc
  case choice {
    g.Sequence([lr, ..rest]) if lr == expected -> #(
      [g.Sequence(list.append(rest, [tail_call])), ..lrs],
      non_lrs,
    )

    anything -> #(lrs, [anything, ..non_lrs])
  }
}
