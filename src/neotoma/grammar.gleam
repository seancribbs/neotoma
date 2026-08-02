/// g <- "neotoma"
/// Position within the input stream
pub type Position {
  Position(offset: Int, line: Int, column: Int)
}

/// Spans cover ranges of positions within the input
pub type Span {
  Span(start: Position, end: Position)
}

/// The kinds of all concrete syntax elements
pub type TerminalKind {
  // Anything
  String(str: String)
}

/// Grammar contains all rules of the language
pub type Grammar {
  Grammar(rules: List(Rule))
}

/// Rules define the way that nonterminals are produced
pub type Rule {
  Rule(name: String, expr: Atomic)
}

// /// A single parsing expression
// pub type Expression {
//   Primary(Primary)
// }

// /// Primary is a sub-expression with optional operators on it (Kleene, lookahead)
// pub type Primary {
//   Atomic(Atomic)
// }

/// An atomic expression contains a single terminal, non-terminal, or parenthesized expression
pub type Atomic {
  Terminal(kind: TerminalKind, span: Span)
}
