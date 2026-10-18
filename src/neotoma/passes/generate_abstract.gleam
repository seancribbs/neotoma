import gleam/int
import gleam/list
import gleam/set
import neotoma/ir/mini_erl as a
import neotoma/ir/norep as g

pub type SuccessCont =
  fn(a.Expr, String, String, String) -> List(a.Expr)

pub type FailCont =
  fn(String) -> List(a.Expr)

const advance_fun_name = "advance_position___"

pub fn generate_abstract_module(
  grammar: g.Grammar,
  memos: set.Set(String),
) -> List(a.Form) {
  list.append(
    [
      a.Module(grammar.name),
      a.Export([#("string", 1)]),
      generate_string_entrypoint(grammar),
      generate_advance_position(),
    ],
    list.map(grammar.rules, generate_rule(_, memos)),
  )
}

pub fn generate_string_entrypoint(grammar: g.Grammar) -> a.Form {
  let assert [g.Definition(name:, ..), ..] = grammar.rules
  let subject =
    a.Apply(a.LocalFunction(name), [
      a.Variable("Input"),
      a.MapConstructor([
        #(a.Atom("offset"), a.Integer(0)),
        #(a.Atom("line"), a.Integer(1)),
        #(a.Atom("column"), a.Integer(1)),
      ]),
      a.MapConstructor([]),
    ])
  a.Function(
    "string",
    a.FunctionClause([a.VariablePattern("Input")], [
      a.Case(subject, [
        a.CaseClause(
          a.TuplePattern([
            a.AtomPattern("ok"),
            a.TuplePattern([
              a.VariablePattern("Result"),
              a.VariablePattern("Remainder"),
              a.VariablePattern("Position"),
              a.Ignore,
            ]),
          ]),
          [
            a.Tuple([
              a.Atom("ok"),
              a.Tuple([
                a.Variable("Result"),
                a.Variable("Remainder"),
                a.Variable("Position"),
              ]),
            ]),
          ],
        ),
        a.CaseClause(
          a.TuplePattern([
            a.AtomPattern("error"),
            a.TuplePattern([
              a.VariablePattern("Error"),
              a.Ignore,
            ]),
          ]),
          [
            a.Tuple([
              a.Atom("error"),
              a.Variable("Error"),
            ]),
          ],
        ),
      ]),
    ]),
  )
}

pub fn generate_rule(
  definition: g.Definition,
  memos: set.Set(String),
) -> a.Form {
  let inner =
    generate_expression(
      definition.expr,
      [#("Input", "Position", "Memo")],
      default_success,
      default_failure,
    )
  let wrapper = case set.contains(memos, definition.name) {
    True -> {
      let nt = a.Atom(definition.name)

      [
        a.Match(
          a.MapPattern([#(a.AtomPattern("offset"), a.VariablePattern("Offset"))]),
          a.Variable("Position"),
        ),
        a.Case(a.Variable("Memo"), [
          // #{ {'temp', Offset} := {ok, {MemoResult, MemoRemainder, MemoPosition}} } ->
          //    {ok, {MemoResult, MemoRemainder, MemoPosition, Memo}};
          a.CaseClause(
            a.MapPattern([
              #(
                a.TuplePattern([
                  a.AtomPattern(definition.name),
                  a.VariablePattern("Offset"),
                ]),
                a.TuplePattern([
                  a.AtomPattern("ok"),
                  a.TuplePattern([
                    a.VariablePattern("MemoResult"),
                    a.VariablePattern("MemoRemainder"),
                    a.VariablePattern("MemoPosition"),
                  ]),
                ]),
              ),
            ]),
            [
              a.Tuple([
                a.Atom("ok"),
                a.Tuple([
                  a.Variable("MemoResult"),
                  a.Variable("MemoRemainder"),
                  a.Variable("MemoPosition"),
                  a.Variable("Memo"),
                ]),
              ]),
            ],
          ),
          a.CaseClause(
            a.MapPattern([
              #(
                a.TuplePattern([
                  a.AtomPattern(definition.name),
                  a.VariablePattern("Offset"),
                ]),
                a.TuplePattern([
                  a.AtomPattern("error"),
                  a.VariablePattern("MemoError"),
                ]),
              ),
            ]),
            [
              a.Tuple([
                a.Atom("error"),
                a.Tuple([
                  a.Variable("MemoError"),
                  a.Variable("Memo"),
                ]),
              ]),
            ],
          ),
          a.CaseClause(a.Ignore, [
            a.Match(a.VariablePattern("ParseResult"), a.Block(inner)),
            a.Case(a.Variable("ParseResult"), [
              a.CaseClause(
                a.TuplePattern([
                  a.AtomPattern("ok"),
                  a.TuplePattern([
                    a.VariablePattern("ParseValue"),
                    a.VariablePattern("ParseRemainder"),
                    a.VariablePattern("ParsePosition"),
                    a.VariablePattern("ParseMemo"),
                  ]),
                ]),
                [
                  a.Tuple([
                    a.Atom("ok"),
                    a.Tuple([
                      a.Variable("ParseValue"),
                      a.Variable("ParseRemainder"),
                      a.Variable("ParsePosition"),
                      a.MapUpdate(a.Variable("ParseMemo"), [
                        #(
                          a.Tuple([
                            nt,
                            a.Variable("Offset"),
                          ]),
                          a.Tuple([
                            a.Atom("ok"),
                            a.Tuple([
                              a.Variable("ParseValue"),
                              a.Variable("ParseRemainder"),
                              a.Variable("ParsePosition"),
                            ]),
                          ]),
                        ),
                      ]),
                    ]),
                  ]),
                ],
              ),
              a.CaseClause(
                a.TuplePattern([
                  a.AtomPattern("error"),
                  a.TuplePattern([
                    a.VariablePattern("ParseError"),
                    a.VariablePattern("ParseMemo"),
                  ]),
                ]),
                [
                  a.Tuple([
                    a.Atom("error"),
                    a.Tuple([
                      a.Variable("ParseError"),
                      a.MapUpdate(a.Variable("ParseMemo"), [
                        #(
                          a.Tuple([
                            nt,
                            a.Variable("Offset"),
                          ]),
                          a.Tuple([a.Atom("error"), a.Variable("ParseError")]),
                        ),
                      ]),
                    ]),
                  ]),
                ],
              ),
            ]),
          ]),
        ]),
      ]
    }
    False -> inner
  }
  a.Function(
    definition.name,
    a.FunctionClause(
      [
        a.VariablePattern("Input"),
        a.VariablePattern("Position"),
        a.VariablePattern("Memo"),
      ],
      wrapper,
    ),
  )
}

fn default_success(value, remainder, position, memo) {
  [
    a.Tuple([
      a.Atom("ok"),
      a.Tuple([
        value,
        a.Variable(remainder),
        a.Variable(position),
        a.Variable(memo),
      ]),
    ]),
  ]
}

fn default_failure(memo) {
  [a.Tuple([a.Atom("error"), a.Tuple([a.Atom("no_match"), a.Variable(memo)])])]
}

pub fn generate_expression(
  expr: g.Expression,
  input_stack: List(#(String, String, String)),
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
  input_stack: List(#(String, String, String)),
  success: SuccessCont,
  failure: FailCont,
) -> List(a.Expr) {
  let assert [#(input, position, memo), ..] = input_stack
  case choices {
    [] -> failure(memo)
    [choice] -> generate_expression(choice, input_stack, success, failure)
    [first, ..rest] -> {
      // case first() of
      //   {ok, result} -> {ok, result};
      //   {error, _} ->
      //      case ... of
      //      %% ...
      // end
      let failure: FailCont = fn(new_memo) {
        generate_choice(
          rest,
          [#(input, position, new_memo), ..input_stack],
          success,
          failure,
        )
      }
      generate_expression(first, input_stack, success, failure)
    }
  }
}

pub fn generate_sequence(
  sequence: List(g.Expression),
  input_stack: List(#(String, String, String)),
  success: SuccessCont,
  failure: FailCont,
) -> List(a.Expr) {
  let assert [#(_, _, memo), ..] = input_stack
  case sequence {
    [] -> failure(memo)
    [item] -> {
      let success: SuccessCont = fn(value, remainder, position, memo) {
        // {ok, {[a, b, c, d], remainder}}
        // [a, b, c, d]
        // [b, c, d]
        // [c, d] <-- [_, d], remainder5
        // [d]
        success(a.ListExpr([value]), remainder, position, memo)
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
      let expr_success: SuccessCont = fn(value, remainder, position, new_memo) {
        let seq_success = fn(seq_value, seq_remainder, seq_position, seq_memo) {
          let assert a.ListExpr(values) = seq_value
          success(
            a.ListExpr([value, ..values]),
            seq_remainder,
            seq_position,
            seq_memo,
          )
        }
        generate_sequence(
          rest,
          [#(remainder, position, new_memo), ..input_stack],
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
  input_stack: List(#(String, String, String)),
  success: SuccessCont,
  failure: FailCont,
) -> List(a.Expr) {
  let assert [#(input, position, _memo), ..] = input_stack
  case p {
    g.Atomic(a) -> generate_atomic(a, input_stack, success, failure)
    g.Assert(expr) -> {
      // When the expression succeeds, restore the input to the original position
      let success: SuccessCont = fn(_result, _remainder, _position, assert_memo) {
        success(a.Binary([]), input, position, assert_memo)
      }
      generate_expression(expr, input_stack, success, failure)
    }
    g.Deny(expr) -> {
      // When the expression fails, restore the input to the original position and continue
      let new_failure: FailCont = fn(deny_memo) {
        success(a.Binary([]), input, position, deny_memo)
      }
      let new_success: SuccessCont = fn(
        _result,
        _remainder,
        _position,
        deny_memo,
      ) {
        failure(deny_memo)
      }
      generate_expression(expr, input_stack, new_success, new_failure)
    }
    g.Optional(expr) -> {
      let failure: FailCont = fn(optional_memo) {
        success(a.Atom("undefined"), input, position, optional_memo)
      }
      generate_expression(expr, input_stack, success, failure)
    }
  }
}

fn generate_atomic(
  atomic: g.Atomic,
  input_stack: List(#(String, String, String)),
  success: SuccessCont,
  failure: FailCont,
) -> List(a.Expr) {
  let assert [input, ..] = input_stack
  let remainder = fresh_variable("_Remainder", input_stack)
  let position = fresh_variable("_Position", input_stack)
  let memo = fresh_variable("_Memo", input_stack)
  let error = fresh_variable("_Error", input_stack)
  let input_stack = [#(remainder, position, memo), ..input_stack]

  case atomic {
    g.Nonterminal(name:) -> {
      let result = fresh_variable("_Result", input_stack)

      // case Name(Input, Position, Memo) of
      //   {ok, {Result, Remainder2, Position2, Memo2}} ->
      //      %% success(Result, Remainder, Position, Memo)
      //      {ok, {Result, Remainder2, Position2, Memo2}};
      //   {error, {Error, Memo2}} ->
      //      %% failure()
      //      {error, {Error, Memo2}}
      // end
      [
        a.Case(
          subject: a.Apply(function: a.LocalFunction(name), arguments: [
            a.Variable(input.0),
            a.Variable(input.1),
            a.Variable(input.2),
          ]),
          clauses: [
            a.CaseClause(
              a.TuplePattern([
                a.AtomPattern("ok"),
                a.TuplePattern([
                  a.VariablePattern(result),
                  a.VariablePattern(remainder),
                  a.VariablePattern(position),
                  a.VariablePattern(memo),
                ]),
              ]),
              success(a.Variable(result), remainder, position, memo),
            ),
            a.CaseClause(
              a.TuplePattern([
                a.AtomPattern("error"),
                a.TuplePattern([
                  a.VariablePattern(error),
                  a.VariablePattern(memo),
                ]),
              ]),
              failure(memo),
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
        //       %% success(..., remainder, position)
        //       Position1 = advance_position___(<<Char/utf8>>, Position),
        //       {ok, {<<Char/utf8>>, Remainder, Position1, Memo}};
        //    <<>> ->
        //       %% failure()
        //       {error, {no_match, Memo}}
        // end
        a.Case(subject: a.Variable(input.0), clauses: [
          a.CaseClause(
            a.BinaryPattern([
              a.BinaryVariable(name: char, types: [a.Utf8]),
              a.BinaryVariable(name: remainder, types: [a.Bytes]),
            ]),
            [
              a.Match(
                a.VariablePattern(position),
                a.Apply(a.LocalFunction(advance_fun_name), [
                  a.Binary([a.BinaryVariable(name: char, types: [a.Utf8])]),
                  a.Variable(input.1),
                ]),
              ),
              ..success(
                a.Binary([a.BinaryVariable(name: char, types: [a.Utf8])]),
                remainder,
                position,
                input.2,
              )
            ],
          ),
          a.CaseClause(a.BinaryPattern([]), failure(input.2)),
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
        a.Case(subject: a.Variable(input.0), clauses: [
          a.CaseClause(
            a.BinaryPattern([
              a.BinaryStringField(str),
              a.BinaryVariable(name: remainder, types: [a.Bytes]),
            ]),
            [
              a.Match(
                a.VariablePattern(position),
                a.Apply(a.LocalFunction(advance_fun_name), [
                  a.Binary([a.BinaryStringField(str)]),
                  a.Variable(input.1),
                ]),
              ),
              ..success(
                a.Binary([a.BinaryStringField(str)]),
                remainder,
                position,
                input.2,
              )
            ],
          ),
          a.CaseClause(a.Ignore, failure(input.2)),
        ]),
      ]
    }
    g.Terminal(kind: g.CharacterClass(chars: _)) ->
      panic as "expand_charclasses pass was skipped"
    g.Epsilon -> {
      success(a.ListExpr([]), input.0, input.1, input.2)
    }
  }
}

fn fresh_variable(prefix: String, stack: List(a)) -> String {
  let counter = list.length(stack) + 1
  prefix <> int.to_string(counter)
}

// advance_position_from_match(Bin, Position) ->
//     #{offset := Offset, line := Line, column := Column} = Position,
//     case Bin of
//         <<>> -> Position;
//         <<"\r\n", Rest/binary>> ->
//             advance_position_from_match(Rest,
//                 #{offset => Offset+2, line => Line+1, column => 1});
//         <<"\n", Rest/binary>> ->
//             advance_position_from_match(Rest,
//                 #{offset => Offset+1, line => Line+1, column => 1 });
//         <<Char/utf8, Rest/binary>> ->
//             CharLen = byte_size(unicode:characters_to_binary([Char], utf8)),
//             advance_position_from_match(Rest,
//                 Position#{offset :=Offset+CharLen, column :=Column+1})
//     end.

fn generate_advance_position() -> a.Form {
  a.Function(
    advance_fun_name,
    a.FunctionClause([a.VariablePattern("Bin"), a.VariablePattern("Position")], [
      a.Match(
        a.MapPattern([
          #(a.AtomPattern("offset"), a.VariablePattern("Offset")),
          #(a.AtomPattern("line"), a.VariablePattern("Line")),
          #(a.AtomPattern("column"), a.VariablePattern("Column")),
        ]),
        a.Variable("Position"),
      ),
      a.Case(a.Variable("Bin"), [
        a.CaseClause(a.BinaryPattern([]), [a.Variable("Position")]),
        a.CaseClause(
          a.BinaryPattern([
            a.BinaryStringField("\r\n"),
            a.BinaryVariable("Rest", [a.Bytes]),
          ]),
          [
            a.Apply(a.LocalFunction(advance_fun_name), [
              a.Variable("Rest"),
              a.MapConstructor([
                #(
                  a.Atom("offset"),
                  a.Infix(a.Variable("Offset"), a.Plus, a.Integer(2)),
                ),
                #(
                  a.Atom("line"),
                  a.Infix(a.Variable("Line"), a.Plus, a.Integer(1)),
                ),
                #(a.Atom("column"), a.Integer(1)),
              ]),
            ]),
          ],
        ),
        a.CaseClause(
          a.BinaryPattern([
            a.BinaryStringField("\n"),
            a.BinaryVariable("Rest", [a.Bytes]),
          ]),
          [
            a.Apply(a.LocalFunction(advance_fun_name), [
              a.Variable("Rest"),
              a.MapConstructor([
                #(
                  a.Atom("offset"),
                  a.Infix(a.Variable("Offset"), a.Plus, a.Integer(1)),
                ),
                #(
                  a.Atom("line"),
                  a.Infix(a.Variable("Line"), a.Plus, a.Integer(1)),
                ),
                #(a.Atom("column"), a.Integer(1)),
              ]),
            ]),
          ],
        ),
        a.CaseClause(
          a.BinaryPattern([
            a.BinaryVariable("Char", [a.Utf8]),
            a.BinaryVariable("Rest", [a.Bytes]),
          ]),
          [
            a.Match(
              a.VariablePattern("CharLen"),
              a.Apply(a.LocalFunction("byte_size"), [
                a.Apply(a.RemoteFunction("unicode", "characters_to_binary"), [
                  a.ListExpr([a.Variable("Char")]),
                  a.Atom("utf8"),
                ]),
              ]),
            ),
            a.Apply(a.LocalFunction(advance_fun_name), [
              a.Variable("Rest"),
              a.MapUpdate(a.Variable("Position"), [
                #(
                  a.Atom("offset"),
                  a.Infix(a.Variable("Offset"), a.Plus, a.Variable("CharLen")),
                ),
                #(
                  a.Atom("column"),
                  a.Infix(a.Variable("Column"), a.Plus, a.Integer(1)),
                ),
              ]),
            ]),
          ],
        ),
      ]),
    ]),
  )
}
