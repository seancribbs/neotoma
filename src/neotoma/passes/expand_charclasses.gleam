import gleam/int
import gleam/list
import gleam/string
import neotoma/ir/grammar

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
  grammar.Grammar(..g, rules: list.map(g.rules, expand_charclasses_def))
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
    grammar.Sequence(exprs, code) ->
      grammar.Sequence(list.map(exprs, expand_charclasses_expr), code)
    grammar.Choice(exprs) ->
      grammar.Choice(list.map(exprs, expand_charclasses_expr))
  }
}

fn expand_charclasses_prim(prim: grammar.Primary) -> grammar.Expression {
  case prim {
    grammar.Atomic(grammar.Terminal(kind: grammar.CharacterClass(entries))) -> {
      entries
      |> list.flat_map(expand_entry)
      |> grammar.Choice
    }
    grammar.Assert(expr) ->
      grammar.Primary(grammar.Assert(expand_charclasses_expr(expr)))
    grammar.Deny(expr) ->
      grammar.Primary(grammar.Deny(expand_charclasses_expr(expr)))
    grammar.Optional(expr) ->
      grammar.Primary(grammar.Optional(expand_charclasses_expr(expr)))
    grammar.ZeroOrMore(expr) ->
      grammar.Primary(grammar.ZeroOrMore(expand_charclasses_expr(expr)))
    grammar.OneOrMore(expr) ->
      grammar.Primary(grammar.OneOrMore(expand_charclasses_expr(expr)))
    grammar.Labeled(expr:, binding:) ->
      grammar.Primary(grammar.Labeled(expand_charclasses_expr(expr), binding))
    _ -> grammar.Primary(prim)
  }
}

fn expand_entry(
  entry: grammar.CharacterClassEntry,
) -> List(grammar.Expression) {
  case entry {
    grammar.SingleCharacter(char:) -> [
      grammar.Primary(
        grammar.Atomic(grammar.Terminal(kind: grammar.String(char))),
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
          grammar.Atomic(
            grammar.Terminal(
              kind: grammar.String(string.from_utf_codepoints([cp])),
            ),
          ),
        ),
        ..acc
      ]
    }
  }
}
