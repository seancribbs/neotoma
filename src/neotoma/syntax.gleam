import gleam/erlang/charlist
import gleam/erlang/atom.{type Atom}

/// Equivalent to erl_syntax:syntaxTree() type
pub type SyntaxTree

// attribute/2
@external(erlang, "erl_syntax", "attribute")
pub fn attribute(name: SyntaxTree, args: List(SyntaxTree)) -> SyntaxTree

// arity_qualifier/2
@external(erlang, "erl_syntax", "arity_qualifier")
pub fn arity_qualifier(body: SyntaxTree, arity: SyntaxTree) -> SyntaxTree

// application/2
@external(erlang, "erl_syntax", "application")
pub fn local_application(operator: SyntaxTree, arguments: List(SyntaxTree)) -> SyntaxTree

// application/3
@external(erlang, "erl_syntax", "application")
pub fn remote_application(module: SyntaxTree, function: SyntaxTree, arguments: List(SyntaxTree)) -> SyntaxTree

// list/1 ([...])
// TODO: create list_cons/2 for ([H, ... | T]) constructions
@external(erlang, "erl_syntax", "list")
pub fn list(items: List(SyntaxTree)) -> SyntaxTree

// form_list/1
@external(erlang, "erl_syntax", "form_list")
pub fn form_list(forms: List(SyntaxTree)) -> SyntaxTree

// function/2
@external(erlang, "erl_syntax", "function")
pub fn function(name: SyntaxTree, clauses: List(SyntaxTree)) -> SyntaxTree

@external(erlang, "erl_syntax", "binary")
pub fn binary(fields: List(SyntaxTree)) -> SyntaxTree

@external(erlang, "erl_syntax", "binary_field")
pub fn binary_field(body: SyntaxTree) -> SyntaxTree

pub fn variable(name: String) -> SyntaxTree {
  do_variable(charlist.from_string(name))
}

@external(erlang, "erl_syntax", "variable")
fn do_variable(chars: charlist.Charlist) -> SyntaxTree

@external(erlang, "erl_syntax", "binary_field")
pub fn binary_field_with_types(body: SyntaxTree, types: List(SyntaxTree)) -> SyntaxTree

pub fn string(string: String) -> SyntaxTree {
  do_string(charlist.from_string(string))
}

@external(erlang, "erl_syntax", "string")
fn do_string(chars: charlist.Charlist) -> SyntaxTree

@external(erlang, "erl_syntax", "case_expr")
pub fn case_expr(subject: SyntaxTree, clauses: List(SyntaxTree)) -> SyntaxTree

@external(erlang, "erl_syntax", "clause")
pub fn clause(
  patterns: List(SyntaxTree),
  guards: List(SyntaxTree),
  body: List(SyntaxTree),
) -> SyntaxTree

pub fn atom(a: String) -> SyntaxTree {
  do_atom(atom.create(a))
}

@external(erlang, "erl_syntax", "atom")
fn do_atom(a: Atom) -> SyntaxTree

@external(erlang, "erl_syntax", "abstract")
pub fn abstract(value: a) -> SyntaxTree

@external(erlang, "erl_syntax", "tuple")
pub fn tuple(tuple: List(SyntaxTree)) -> SyntaxTree

@external(erlang, "erl_syntax", "underscore")
pub fn underscore() -> SyntaxTree

@external(erlang, "neotoma_ffi", "format")
pub fn format(tree: SyntaxTree) -> String
