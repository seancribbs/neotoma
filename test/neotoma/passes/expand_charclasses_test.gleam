import neotoma/passes/expand_charclasses
import neotoma/grammar as g

const span = g.Span(g.Position(0,0,0), g.Position(1,1,0))

pub fn expand_charclasses_single_chars_test() {
  // punct <- [_-]
  let g1 = g.Grammar("punct", [
    g.Definition("punct", g.Primary(g.Atomic(g.Terminal(kind: g.CharacterClass([
      g.SingleCharacter("_"),
      g.SingleCharacter("-"),
    ]), span:))))
  ])

  let g2 = expand_charclasses.expand_charclasses(g1)
  assert g2 == g.Grammar("punct", [
    g.Definition("punct", g.Choice([
      g.Primary(g.Atomic(g.Terminal(g.String("_"), span))),
      g.Primary(g.Atomic(g.Terminal(g.String("-"), span)))
    ]))
  ])
}

pub fn expand_charclasses_single_range_test() {
  // punct <- [a-b]
  let g1 = g.Grammar("punct", [
    g.Definition("punct", g.Primary(g.Atomic(g.Terminal(kind: g.CharacterClass([
      g.CharacterRange("a", "b")
    ]), span:))))
  ])

  let g2 = expand_charclasses.expand_charclasses(g1)
  assert g2 == g.Grammar("punct", [
    g.Definition("punct", g.Choice([
      g.Primary(g.Atomic(g.Terminal(g.String("b"), span))),
      g.Primary(g.Atomic(g.Terminal(g.String("a"), span))),
    ]))
  ])
}

pub fn expand_charclasses_ranges_and_chars_test() {
  // punct <- [a-b*+]
  let g1 = g.Grammar("punct", [
    g.Definition("punct", g.Primary(g.Atomic(g.Terminal(kind: g.CharacterClass([
      g.CharacterRange("a", "b"),
      g.SingleCharacter("*"),
      g.SingleCharacter("+")
    ]), span:))))
  ])

  let g2 = expand_charclasses.expand_charclasses(g1)
  assert g2 == g.Grammar("punct", [
    g.Definition("punct", g.Choice([
      g.Primary(g.Atomic(g.Terminal(g.String("b"), span))),
      g.Primary(g.Atomic(g.Terminal(g.String("a"), span))),
      g.Primary(g.Atomic(g.Terminal(g.String("*"), span))),
      g.Primary(g.Atomic(g.Terminal(g.String("+"), span))),
    ]))
  ])
}
