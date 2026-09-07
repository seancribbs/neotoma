//// The IR of the input grammar

/// The kinds of all concrete syntax elements.
///
/// All terminals are in UTF-8 encoding. If we want to
/// later support other encodings, the encoding will need
/// to be specified at grammar definition time or invocation
/// of the parse, the latter being much more complicated.
pub type TerminalKind {
  Anything
  // "." operator
  String(str: String)
  // Literal string
  CharacterClass(chars: List(CharacterClassEntry))
  // character class as in regular expressions, e.g. [A-Z0-9_]
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
  // zero-or-more repetition (*)
  ZeroOrMore(Expression)
  // one-or-more repetition (+)
  OneOrMore(Expression)
}

/// An atomic expression contains a single terminal, non-terminal, or parenthesized expression
pub type Atomic {
  Nonterminal(name: String)
  Terminal(kind: TerminalKind)
}
