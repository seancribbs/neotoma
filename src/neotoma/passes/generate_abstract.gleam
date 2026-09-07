import gleam/int
import gleam/list
import neotoma/ir/mini_erl as a
import neotoma/ir/norep as g

pub type SuccessCont =
  fn(a.Expr, String) -> List(a.Expr)

pub type FailCont =
  fn() -> List(a.Expr)

pub fn generate_abstract_module(grammar: g.Grammar) -> List(a.Form) {
  list.append(
    [
      a.Module(grammar.name),
      a.Export([#("string", 1)]),
      generate_string_entrypoint(grammar),
    ],
    list.map(grammar.rules, generate_rule),
  )
}

pub fn generate_string_entrypoint(grammar: g.Grammar) -> a.Form {
  let assert [g.Definition(name:, ..), ..] = grammar.rules
  a.Function(
    "string",
    a.FunctionClause([a.VariablePattern("Input")], [
      a.Apply(a.LocalFunction(name), [a.Variable("Input")]),
    ]),
  )
}

pub fn generate_rule(definition: g.Definition) -> a.Form {
  a.Function(
    definition.name,
    a.FunctionClause(
      [a.VariablePattern("Input")],
      generate_expression(
        definition.expr,
        ["Input"],
        default_success,
        default_failure,
      ),
    ),
  )
}

fn default_success(value, remainder) {
  [a.Tuple([a.Atom("ok"), a.Tuple([value, a.Variable(remainder)])])]
}

fn default_failure() {
  [a.Tuple([a.Atom("error"), a.Atom("no_match")])]
}

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
        generate_sequence(
          rest,
          [remainder, ..input_stack],
          seq_success,
          failure,
        )
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
    g.Assert(expr) -> {
      // When the expression succeeds, restore the input to the original position
      let assert [input, ..] = input_stack
      let success: SuccessCont = fn(_result, _remainder) {
        success(a.Binary([]), input)
      }
      generate_expression(expr, input_stack, success, failure)
    }
    g.Deny(expr) -> {
      // When the expression fails, restore the input to the original position and continue
      let assert [input, ..] = input_stack
      let new_failure: FailCont = fn() { success(a.Binary([]), input) }
      let new_success: SuccessCont = fn(_result, _remainder) { failure() }
      generate_expression(expr, input_stack, new_success, new_failure)
    }
    g.Optional(expr) -> {
      let assert [input, ..] = input_stack
      let failure: FailCont = fn() { success(a.Atom("undefined"), input) }
      generate_expression(expr, input_stack, success, failure)
    }
  }
}

fn generate_atomic(
  atomic: g.Atomic,
  input_stack: List(String),
  success: SuccessCont,
  failure: FailCont,
) -> List(a.Expr) {
  let assert [input, ..] = input_stack
  let remainder = fresh_variable("_Remainder", input_stack)
  let input_stack = [remainder, ..input_stack]

  case atomic {
    g.Nonterminal(name:) -> {
      let result = fresh_variable("_Result", input_stack)
      // case Name(Input) of
      //   {ok, {Result, Remainder}} ->
      //      %% success(Result, Remainder)
      //      {ok, {Result, Remainder}};
      //   {error, _Error} ->
      //      %% failure()
      //      {error, no_match}
      // end
      [
        a.Case(
          subject: a.Apply(function: a.LocalFunction(name), arguments: [
            a.Variable(input),
          ]),
          clauses: [
            a.CaseClause(
              a.TuplePattern([
                a.AtomPattern("ok"),
                a.TuplePattern([
                  a.VariablePattern(result),
                  a.VariablePattern(remainder),
                ]),
              ]),
              success(a.Variable(result), remainder),
            ),
            a.CaseClause(
              a.TuplePattern([a.AtomPattern("error"), a.Ignore]),
              failure(),
            ),
          ],
        ),
      ]
    }
    g.Terminal(kind: g.Anything) -> {
      let char = fresh_variable("_Char", input_stack)
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
    g.Terminal(kind: g.String(str:)) -> {
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
    g.Terminal(kind: g.CharacterClass(chars: _)) ->
      panic as "expand_charclasses pass was skipped"
    g.Epsilon -> {
      success(a.ListExpr([]), input)
    }
  }
}

fn fresh_variable(prefix: String, stack: List(a)) -> String {
  let counter = list.length(stack) + 1
  prefix <> int.to_string(counter)
}
