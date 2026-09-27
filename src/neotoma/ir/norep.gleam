//// Intermediate representation of the grammar with Kleene operators (repetition) removed.
//// At this step, all repetition has been rewritten using right-recursive rules.

import gleam/list

/// The kinds of all concrete syntax elements.
///
/// All terminals are in UTF-8 encoding. If we want to
/// later support other encodings, the encoding will need
/// to be specified at grammar definition time or invocation
/// of the parse, the latter being much more complicated.
pub type TerminalKind {
  // "." operator
  Anything
  // Literal string
  String(str: String)
  // character class as in regular expressions, e.g. [A-Z0-9_]
  CharacterClass(chars: List(CharacterClassEntry))
}

pub type CharacterClassEntry {
  SingleCharacter(char: String)
  CharacterRange(start: String, end: String)
}

/// Grammar contains all rules of the language
pub type Grammar {
  Grammar(name: String, rules: List(Definition))
}

/// Definitions define the way that nonterminals are produced
pub type Definition {
  Definition(name: String, expr: Expression)
}

/// A single parsing expression
pub type Expression {
  Primary(Primary)
  Sequence(List(Expression))
  Choice(List(Expression))
}

/// Primary is a sub-expression with optional operators on it (Kleene,
/// lookahead)
pub type Primary {
  // Undecorated
  Atomic(Atomic)
  // zero-width positive lookahead
  Assert(Expression)
  // zero-width negative lookahead
  Deny(Expression)
  // optional sub-expression, if it fails, it is skipped
  Optional(Expression)
}

/// An atomic expression contains a single terminal, non-terminal, or parenthesized expression
pub type Atomic {
  Nonterminal(name: String)
  Terminal(kind: TerminalKind)
  Epsilon
}

/// Computes the "size" (cost) of a parsing expression
pub fn size(expr: Expression) -> Int {
  case expr {
    Primary(prim) -> size_of_primary(prim)
    Sequence(items) -> list.fold(items, 1, fn(acc, item) { acc + size(item) })
    Choice(alts) -> list.fold(alts, 1, fn(acc, item) { acc + size(item) })
  }
}

fn size_of_primary(prim: Primary) -> Int {
  case prim {
    Atomic(Nonterminal(_)) -> 1
    // TODO: examine the cost of character classes
    Atomic(Terminal(_)) -> 1
    Atomic(Epsilon) -> 0
    Assert(expr) -> size(expr)
    Deny(expr) -> size(expr)
    Optional(expr) -> 1 + size(expr)
  }
}
