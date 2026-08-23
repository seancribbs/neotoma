import gleam/int
import gleam/list
import gleam/string
import neotoma/grammar

// "trie"
//
// "<" / "<=" / "<==" / "<<=" / "<<<"
// <
//   =
//    =
//      FatLeftArrow
//   LessThanEqual
//   <
//
// LT
//
// case Input of
//   <<"">>

pub fn expand_charclasses(g: grammar.Grammar) -> grammar.Grammar {
  g.rules
  |> list.map(expand_charclasses_def)
  |> grammar.Grammar()
}

fn expand_charclasses_def(
  definition: grammar.Definition,
) -> grammar.Definition {
  grammar.Definition(
    ..definition,
    expr: expand_charclasses_expr(definition.expr),
  )
}

fn expand_charclasses_expr(expr: grammar.Expression) -> grammar.Expression {
  case expr {
    grammar.Primary(prim) -> expand_charclasses_prim(prim)
    grammar.Sequence(exprs) ->
      grammar.Sequence(list.map(exprs, expand_charclasses_expr))
    grammar.Choice(exprs) ->
      grammar.Choice(list.map(exprs, expand_charclasses_expr))
  }
}

fn expand_charclasses_prim(prim: grammar.Primary) -> grammar.Expression {
  case prim {
    grammar.Atomic(grammar.Terminal(
      kind: grammar.CharacterClass(entries),
      span:,
    )) -> {
      entries
      |> list.flat_map(expand_entry(_, span))
      |> grammar.Choice
    }
    _ -> grammar.Primary(prim)
  }
}

fn expand_entry(
  entry: grammar.CharacterClassEntry,
  span: grammar.Span,
) -> List(grammar.Expression) {
  case entry {
    grammar.SingleCharacter(char:) -> [
      grammar.Primary(
        grammar.Atomic(grammar.Terminal(kind: grammar.String(char), span:)),
      ),
    ]
    grammar.CharacterRange(start:, end:) -> {
      let assert [start] = string.to_utf_codepoints(start)
      let assert [end] = string.to_utf_codepoints(end)
      let start = string.utf_codepoint_to_int(start)
      let end = string.utf_codepoint_to_int(end)
      use acc, cp <- int.range(from: start, to: end + 1, with: [])
      let assert Ok(cp) = string.utf_codepoint(cp)
      [
        grammar.Primary(
          grammar.Atomic(grammar.Terminal(
            kind: grammar.String(string.from_utf_codepoints([cp])),
            span:,
          )),
        ),
        ..acc
      ]
    }
  }
}
