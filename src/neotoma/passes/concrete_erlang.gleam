import gleam/list
import neotoma/abstract as a
import neotoma/syntax

pub fn lower(forms: List(a.Form)) -> syntax.SyntaxTree {
  forms
  |> list.map(lower_form)
  |> syntax.form_list
}

fn lower_form(form: a.Form) -> syntax.SyntaxTree {
  case form {
    a.Module(name:) ->
      syntax.attribute(syntax.atom("module"), [syntax.atom(name)])
    a.Export(funs:) ->
      syntax.attribute(syntax.atom("export"), [
        syntax.list(list.map(funs, lower_arity)),
      ])
    a.Function(name:, clause:) ->
      syntax.function(syntax.atom(name), [lower_function_clause(clause)])
  }
}

fn lower_function_clause(clause: a.FunctionClause) -> syntax.SyntaxTree {
  syntax.clause(
    list.map(clause.arguments, lower_pattern),
    [],
    list.map(clause.body, lower_expr),
  )
}

fn lower_expr(expr: a.Expr) -> syntax.SyntaxTree {
  case expr {
    a.Atom(atom) -> syntax.atom(atom)
    a.Variable(var) -> syntax.variable(var)
    a.Tuple(elements) -> syntax.tuple(list.map(elements, lower_expr))
    a.ListExpr(items) -> syntax.list(list.map(items, lower_expr))
    a.Binary(fields) -> syntax.binary(list.map(fields, lower_binary_field))
    a.Case(subject:, clauses:) ->
      syntax.case_expr(
        lower_expr(subject),
        list.map(clauses, lower_case_clause),
      )
    a.Apply(function: a.LocalFunction(name:), arguments:) ->
      syntax.local_application(
        syntax.atom(name),
        list.map(arguments, lower_expr),
      )
    a.Apply(function: a.RemoteFunction(module:, name:), arguments:) ->
      syntax.remote_application(
        syntax.atom(module),
        syntax.atom(name),
        list.map(arguments, lower_expr),
      )
  }
}

fn lower_case_clause(case_clause: a.CaseClause) -> syntax.SyntaxTree {
  syntax.clause(
    [lower_pattern(case_clause.pattern)],
    [],
    list.map(case_clause.body, lower_expr),
  )
}

fn lower_pattern(pattern: a.Pattern) -> syntax.SyntaxTree {
  case pattern {
    a.AtomPattern(atom) -> syntax.atom(atom)
    a.TuplePattern(elements) -> syntax.tuple(list.map(elements, lower_pattern))
    a.VariablePattern(variable) -> syntax.variable(variable)
    a.BinaryPattern(fields) ->
      syntax.binary(list.map(fields, lower_binary_field))
    a.Ignore -> syntax.variable("_")
  }
}

fn lower_binary_field(binary_field: a.BinaryField) -> syntax.SyntaxTree {
  case binary_field {
    a.BinaryStringField(string:) -> syntax.binary_field(syntax.string(string))
    a.BinaryVariable(name:, types:) ->
      syntax.binary_field_with_types(
        syntax.variable(name),
        list.map(types, lower_binary_field_type),
      )
  }
}

fn lower_binary_field_type(
  binary_field_type: a.BinaryFieldType,
) -> syntax.SyntaxTree {
  case binary_field_type {
    a.Utf8 -> syntax.atom("utf8")
    a.Bytes -> syntax.atom("bytes")
  }
}

fn lower_arity(value: #(String, Int)) -> syntax.SyntaxTree {
  syntax.arity_qualifier(syntax.atom(value.0), syntax.abstract(value.1))
}
