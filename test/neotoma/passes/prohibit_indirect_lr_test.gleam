import neotoma/ir/norep as g
import neotoma/passes/prohibit_indirect_lr

pub fn prohibit_indirect_lr_simple_test() {
  let input =
    g.Grammar("prohibit_indirect_lr_simple_test", [
      g.Definition(
        "a",
        g.Choice([
          g.Primary(g.Atomic(g.Nonterminal("b"))),
          g.Primary(g.Atomic(g.Nonterminal("c"))),
        ]),
      ),
      g.Definition(
        "b",
        g.Sequence([
          g.Primary(g.Atomic(g.Nonterminal("a"))),
          g.Primary(g.Atomic(g.Terminal(g.String(".")))),
        ]),
      ),
      g.Definition("c", g.Primary(g.Atomic(g.Terminal(g.String("foo"))))),
    ])

  assert Error(prohibit_indirect_lr.CyclesDetected([["a", "b", "a"]]))
    == prohibit_indirect_lr.prohibit_indirect_lr(input)
}

// 1. Recursing into look-ahead or optional primaries
pub fn prohibit_indirect_lr_lookahead_test() {
  let input =
    g.Grammar("prohibit_indirect_lr_lookahead_test_assert", [
      g.Definition(
        "a",
        g.Choice([
          g.Primary(g.Atomic(g.Nonterminal("b"))),
          g.Primary(g.Atomic(g.Nonterminal("c"))),
        ]),
      ),
      g.Definition(
        "b",
        g.Sequence([
          g.Primary(g.Assert(g.Primary(g.Atomic(g.Nonterminal("a"))))),
          g.Primary(g.Atomic(g.Terminal(g.String(".")))),
        ]),
      ),
      g.Definition("c", g.Primary(g.Atomic(g.Terminal(g.String("foo"))))),
    ])

  assert Error(prohibit_indirect_lr.CyclesDetected([["a", "b", "a"]]))
    == prohibit_indirect_lr.prohibit_indirect_lr(input)

  let input =
    g.Grammar("prohibit_indirect_lr_lookahead_test_deny", [
      g.Definition(
        "a",
        g.Choice([
          g.Primary(g.Atomic(g.Nonterminal("b"))),
          g.Primary(g.Atomic(g.Nonterminal("c"))),
        ]),
      ),
      g.Definition(
        "b",
        g.Sequence([
          g.Primary(g.Deny(g.Primary(g.Atomic(g.Nonterminal("a"))))),
          g.Primary(g.Atomic(g.Terminal(g.String(".")))),
        ]),
      ),
      g.Definition("c", g.Primary(g.Atomic(g.Terminal(g.String("foo"))))),
    ])

  assert Error(prohibit_indirect_lr.CyclesDetected([["a", "b", "a"]]))
    == prohibit_indirect_lr.prohibit_indirect_lr(input)
}

pub fn prohibit_indirect_lr_optional_test() {
  let input =
    g.Grammar("prohibit_indirect_lr_optional_test", [
      g.Definition(
        "a",
        g.Choice([
          g.Primary(g.Atomic(g.Nonterminal("b"))),
          g.Primary(g.Atomic(g.Nonterminal("c"))),
        ]),
      ),
      g.Definition(
        "b",
        g.Sequence([
          g.Primary(g.Optional(g.Primary(g.Atomic(g.Nonterminal("a"))))),
          g.Primary(g.Atomic(g.Terminal(g.String(".")))),
        ]),
      ),
      g.Definition("c", g.Primary(g.Atomic(g.Terminal(g.String("foo"))))),
    ])

  assert Error(prohibit_indirect_lr.CyclesDetected([["a", "b", "a"]]))
    == prohibit_indirect_lr.prohibit_indirect_lr(input)
}

// 2. Right-recursion is allowed (no cycle)
pub fn prohibit_indirect_lr_right_recursion_allowed_test() {
  let input =
    g.Grammar("prohibit_indirect_lr_right_recursion_allowed_test", [
      g.Definition(
        "a",
        g.Choice([
          g.Primary(g.Atomic(g.Nonterminal("b"))),
          g.Primary(g.Atomic(g.Nonterminal("c"))),
        ]),
      ),
      g.Definition(
        "b",
        g.Sequence([
          g.Primary(g.Atomic(g.Terminal(g.String(".")))),
          g.Primary(g.Atomic(g.Nonterminal("a"))),
        ]),
      ),
      g.Definition("c", g.Primary(g.Atomic(g.Terminal(g.String("foo"))))),
    ])

  assert Ok(input) == prohibit_indirect_lr.prohibit_indirect_lr(input)
}

// 3. In a choice, not the first choice is indirect LR
pub fn prohibit_indirect_lr_multiple_choice_test() {
  let input =
    g.Grammar("prohibit_indirect_lr_multiple_choice_test", [
      g.Definition(
        "a",
        g.Choice([
          g.Primary(g.Atomic(g.Nonterminal("c"))),
          g.Primary(g.Atomic(g.Nonterminal("b"))),
        ]),
      ),
      g.Definition(
        "b",
        g.Sequence([
          g.Primary(g.Atomic(g.Nonterminal("a"))),
          g.Primary(g.Atomic(g.Terminal(g.String(".")))),
        ]),
      ),
      g.Definition("c", g.Primary(g.Atomic(g.Terminal(g.String("foo"))))),
    ])

  assert Error(prohibit_indirect_lr.CyclesDetected([["a", "b", "a"]]))
    == prohibit_indirect_lr.prohibit_indirect_lr(input)
}
