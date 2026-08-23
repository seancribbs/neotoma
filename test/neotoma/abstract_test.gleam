import neotoma/abstract as a

pub fn abstract_form_example_test() {
  let success_branch = a.CaseClause(a.BinaryPattern([
    a.BinaryStringField("neotoma"),
    a.BinaryVariable("Rest", [a.Bytes]),
  ]),
  [
    a.Tuple([
      a.Atom("ok"),
      a.Tuple([
        a.Binary([a.BinaryStringField("neotoma")]),
        a.Variable("Rest")
      ])
    ])
  ])
  let fail_branch = a.CaseClause(a.VariablePattern("_"), [a.Tuple([a.Atom("error"), a.Atom("no_match")])])
  let case_expr = a.Case(a.Variable("Input"), [success_branch, fail_branch])
  let fun = a.Function("g", a.FunctionClause([a.VariablePattern("Input")], [case_expr]))
  let module: List(a.Form) = [
    // Module("g")
    // Export([#("string", 1)])
    // Function("string", ...)
    fun
  ]
  echo module
}
