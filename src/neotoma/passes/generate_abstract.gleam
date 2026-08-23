import gleam/int
import gleam/list
import neotoma/abstract as a
import neotoma/grammar as g

pub type SuccessCont =
  fn(a.Expr, String) -> List(a.Expr)

pub type FailCont =
  fn() -> List(a.Expr)

pub fn generate_expression(
  expr: g.Expression,
  input_stack: List(String),
  success: SuccessCont,
  failure: FailCont,
) -> List(a.Expr) {
  case expr {
    g.Primary(p) -> generate_primary(p, input_stack, success, failure)
    g.Sequence(list) -> generate_sequence(list, input_stack, success, failure)
    g.Choice(choices) -> generate_choice(choices, input_stack, success, failure)
  }
}

pub fn generate_choice(
  choices: List(g.Expression),
  input_stack: List(String),
  success: fn(a.Expr, String) -> List(a.Expr),
  failure: fn() -> List(a.Expr),
) -> List(a.Expr) {
  case choices {
    [] -> failure()
    [choice] -> generate_expression(choice, input_stack, success, failure)
    [first, ..rest] -> {
      // case first() of
      //   {ok, result} -> {ok, result};
      //   {error, _} ->
      //      case ... of
      //      %% ...
      // end
      let failure = fn() {
        generate_choice(rest, input_stack, success, failure)
      }
      generate_expression(first, input_stack, success, failure)
    }
  }
}

pub fn generate_sequence(
  sequence: List(g.Expression),
  input_stack: List(String),
  success: SuccessCont,
  failure: FailCont,
) -> List(a.Expr) {
  case sequence {
    [] -> failure()
    [item] -> {
      let success = fn(value, remainder) {
        // {ok, {[a, b, c, d], remainder}}
        // [a, b, c, d]
        // [b, c, d]
        // [c, d] <-- [_, d], remainder5
        // [d]
        success(a.ListExpr([value]), remainder)
      }
      generate_expression(item, input_stack, success, failure)
    }
    [first, ..rest] -> {
      // case first() of
      //   {ok, the_thing} ->
      //      case ... of
      //      %% ...
      //   {error, _} = Err ->
      //      Err
      // end
      let expr_success = fn(value, remainder) {
        let seq_success = fn(seq_value, seq_remainder) {
          let assert a.ListExpr(values) = seq_value
          success(a.ListExpr([value, ..values]), seq_remainder)
        }
        generate_sequence(rest, [remainder, ..input_stack], seq_success, failure)
      }
      generate_expression(first, input_stack, expr_success, failure)
    }
  }
}

fn generate_primary(
  p: g.Primary,
  input_stack: List(String),
  success: SuccessCont,
  failure: FailCont,
) -> List(a.Expr) {
  case p {
    g.Atomic(a) -> generate_atomic(a, input_stack, success, failure)
  }
}

fn generate_atomic(
  atomic: g.Atomic,
  input_stack: List(String),
  success: fn(a.Expr, String) -> List(a.Expr),
  failure: fn() -> List(a.Expr),
) -> List(a.Expr) {
  let assert [input, ..] = input_stack
  let remainder = fresh_variable("Remainder", input_stack)
  let input_stack = [remainder, ..input_stack]

  case atomic {
    g.Terminal(kind: g.Anything, span: _) -> {
      let char = fresh_variable("Char", input_stack)
      [
        // case Input of
        //    <<Char/utf8, Remainder/bytes>> ->
        //       %% success(..., remainder)
        //       {ok, {<<Char/utf8>>, Remainder}};
        //    <<>> ->
        //       %% failure()
        //       {error, no_match}
        // end
        a.Case(subject: a.Variable(input), clauses: [
          a.CaseClause(
            a.BinaryPattern([
              a.BinaryVariable(name: char, types: [a.Utf8]),
              a.BinaryVariable(name: remainder, types: [a.Bytes]),
            ]),
            success(
              a.Binary([a.BinaryVariable(name: char, types: [a.Utf8])]),
              remainder,
            ),
          ),
          a.CaseClause(a.BinaryPattern([]), failure()),
        ]),
      ]
    }
    g.Terminal(kind: g.String(str:), span: _) -> {
      // case Input of
      //    <<"str", Remainder/bytes>> ->
      //       %% success(..., remainder)
      //       {ok, {<<"str">>, Remainder}};
      //    _ ->
      //       %% failure()
      //       {error, no_match}
      // end
      [
        a.Case(subject: a.Variable(input), clauses: [
          a.CaseClause(
            a.BinaryPattern([
              a.BinaryStringField(str),
              a.BinaryVariable(name: remainder, types: [a.Bytes]),
            ]),
            success(a.Binary([a.BinaryStringField(str)]), remainder),
          ),
          a.CaseClause(a.Ignore, failure()),
        ]),
      ]
    }
    g.Terminal(kind: g.CharacterClass(chars: _), span: _) ->
      panic as "expand_charclasses pass was skipped"
  }
}

fn fresh_variable(prefix: String, stack: List(a)) -> String {
  let counter = list.length(stack) + 1
  prefix <> int.to_string(counter)
}
