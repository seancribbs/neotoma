import neotoma/syntax
import neotoma/ir/mini_erl

/// Represents a piece of code attached to the syntax tree
pub type Code {
  Unparsed(String)
  Generated(mini_erl.Expr)
}

pub type ParsedCode {
  Parsed(List(syntax.SyntaxTree))
  GeneratedCode(mini_erl.Expr)
}

pub type TopLevelCode {
  TopLevelCode(List(syntax.SyntaxTree))
}
