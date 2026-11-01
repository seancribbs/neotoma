import gleam/list
import gleam/option
import gleam/result
import neotoma/code
import neotoma/ir/norep as g
import neotoma/ir/norep_code as out
import neotoma/syntax

pub fn process_code_blocks(input: g.Grammar) -> Result(out.Grammar, Nil) {
  use top_level_code <- result.try(parse_forms(input.top_level_code))
  use rules <- result.map(list.try_map(input.rules, parse_rule_code))
  out.Grammar(name: input.name, rules:, top_level_code:)
}

fn parse_forms(
  top_level_code: option.Option(String),
) -> Result(option.Option(code.TopLevelCode), Nil) {
  case top_level_code {
    option.Some(str) -> {
      str
      |> syntax.parse_forms()
      |> result.map(code.TopLevelCode)
      |> result.map(option.Some)
    }
    option.None -> Ok(option.None)
  }
}

fn parse_rule_code(definition: g.Definition) -> Result(out.Definition, Nil) {
  use expr <- result.map(parse_expr_code(definition.expr))
  out.Definition(name: definition.name, expr:)
}

fn parse_expr_code(expr: g.Expression) -> Result(out.Expression, Nil) {
  case expr {
    g.Primary(primary:) -> result.map(parse_primary_code(primary), out.Primary)
    g.Sequence(exprs:, code:) -> {
      use exprs <- result.try(list.try_map(exprs, parse_expr_code))
      case option.map(code, parse_inline_code) {
        option.Some(Error(Nil)) -> Error(Nil)
        option.Some(Ok(parsed_code)) ->
          Ok(out.Sequence(exprs:, code: option.Some(parsed_code)))
        option.None -> Ok(out.Sequence(exprs, option.None))
      }
    }
    g.Choice(exprs) ->
      list.try_map(exprs, parse_expr_code) |> result.map(out.Choice)
  }
}

fn parse_primary_code(primary: g.Primary) -> Result(out.Primary, Nil) {
  case primary {
    g.Atomic(g.Nonterminal(name:)) -> Ok(out.Atomic(out.Nonterminal(name:)))
    g.Atomic(g.Terminal(kind: g.Anything)) ->
      Ok(out.Atomic(out.Terminal(out.Anything)))
    g.Atomic(g.Terminal(kind: g.String(str:))) ->
      Ok(out.Atomic(out.Terminal(out.String(str))))
    g.Atomic(g.Terminal(kind: g.CharacterClass(chars:))) ->
      Ok(
        out.Atomic(
          out.Terminal(
            out.CharacterClass(chars: list.map(chars, transform_char_class)),
          ),
        ),
      )
    g.Atomic(g.Epsilon) -> Ok(out.Atomic(out.Epsilon))
    g.Assert(expr) -> result.map(parse_expr_code(expr), out.Assert)
    g.Deny(expr) -> result.map(parse_expr_code(expr), out.Deny)
    g.Optional(expr) -> result.map(parse_expr_code(expr), out.Optional)
    g.Labeled(expr, binding) ->
      result.map(parse_expr_code(expr), out.Labeled(_, binding))
  }
}

fn transform_char_class(
  character_class_entry: g.CharacterClassEntry,
) -> out.CharacterClassEntry {
  case character_class_entry {
    g.SingleCharacter(char:) -> out.SingleCharacter(char:)
    g.CharacterRange(start:, end:) -> out.CharacterRange(start:, end:)
  }
}

fn parse_inline_code(code: code.Code) -> Result(code.ParsedCode, Nil) {
  case code {
    code.Unparsed(str) -> syntax.parse_exprs(str) |> result.map(code.Parsed)
    code.Generated(contents) -> Ok(code.GeneratedCode(contents))
  }
}
