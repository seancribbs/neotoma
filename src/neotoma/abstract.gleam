/// Limited abstract syntax for Erlang code-generation.
///
/// Language constructs that we don't use in the code-generation
/// are explicitly omitted.

/// Top-level syntactic construct. Each module is a list of forms.
pub type Form {
  Module(name: String)
  Export(funs: List(#(String, Int)))
  Function(name: String, clause: FunctionClause)
}

/// Expressions evaluate to values
pub type Expr {
  Atom(String)
  Variable(String)
  Tuple(List(Expr))
  ListExpr(List(Expr))
  Binary(List(BinaryField))
  Case(subject: Expr, clauses: List(CaseClause))
  Apply(function: FunctionRef, arguments: List(Expr))
}

/// How to name a function when applying, either a locally named function
/// or one from another module.
pub type FunctionRef {
  LocalFunction(name: String)
  RemoteFunction(module: String, name: String)
}

/// A single clause of a function definition.
/// Guards are explicitly omitted for now.
pub type FunctionClause {
  FunctionClause(
    arguments: List(Pattern),
    // guard: List(GuardExpr),
    body: List(Expr)
  )
}

/// A single clause/branch of a case expression
pub type CaseClause {
  CaseClause(
    pattern: Pattern,
    // guard: List(GuardExpr),
    body: List(Expr)
  )
}

// pub type GuardExpr

/// Sub-type of expressions that occur on the left-hand side (clauses and matches)
pub type Pattern {
  AtomPattern(String)
  TuplePattern(List(Pattern))
  VariablePattern(String)
  BinaryPattern(List(BinaryField))
  Ignore
}

/// Portion of a binary, either in a pattern or a construction
pub type BinaryField {
  BinaryStringField(string: String)
  BinaryVariable(name: String, types: List(BinaryFieldType))
}

pub type BinaryFieldType {
  Utf8
  Bytes // bytes
}
