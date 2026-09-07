/// Position within the input stream
pub type Position {
  Position(offset: Int, line: Int, column: Int)
}

/// Spans cover ranges of positions within the input
pub type Span {
  Span(start: Position, end: Position)
}
