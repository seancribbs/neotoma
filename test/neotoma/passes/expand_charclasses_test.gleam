import neotoma/ir/grammar as g
import neotoma/passes/expand_charclasses

pub fn expand_charclasses_single_chars_test() {
  // punct <- [_-]
  let g1 =
    g.Grammar("punct", [
      g.Definition(
        "punct",
        g.Primary(
          g.Atomic(
            g.Terminal(
              kind: g.CharacterClass([
                g.SingleCharacter("_"),
                g.SingleCharacter("-"),
              ]),
            ),
          ),
        ),
      ),
    ])

  let g2 = expand_charclasses.expand_charclasses(g1)
  assert g2
    == g.Grammar("punct", [
      g.Definition(
        "punct",
        g.Choice([
          g.Primary(g.Atomic(g.Terminal(g.String("_")))),
          g.Primary(g.Atomic(g.Terminal(g.String("-")))),
        ]),
      ),
    ])
}

pub fn expand_charclasses_single_range_test() {
  // punct <- [a-b]
  let g1 =
    g.Grammar("punct", [
      g.Definition(
        "punct",
        g.Primary(
          g.Atomic(
            g.Terminal(kind: g.CharacterClass([g.CharacterRange("a", "b")])),
          ),
        ),
      ),
    ])

  let g2 = expand_charclasses.expand_charclasses(g1)
  assert g2
    == g.Grammar("punct", [
      g.Definition(
        "punct",
        g.Choice([
          g.Primary(g.Atomic(g.Terminal(g.String("b")))),
          g.Primary(g.Atomic(g.Terminal(g.String("a")))),
        ]),
      ),
    ])
}

pub fn expand_charclasses_ranges_and_chars_test() {
  // punct <- [a-b*+]
  let g1 =
    g.Grammar("punct", [
      g.Definition(
        "punct",
        g.Primary(
          g.Atomic(
            g.Terminal(
              kind: g.CharacterClass([
                g.CharacterRange("a", "b"),
                g.SingleCharacter("*"),
                g.SingleCharacter("+"),
              ]),
            ),
          ),
        ),
      ),
    ])

  let g2 = expand_charclasses.expand_charclasses(g1)
  assert g2
    == g.Grammar("punct", [
      g.Definition(
        "punct",
        g.Choice([
          g.Primary(g.Atomic(g.Terminal(g.String("b")))),
          g.Primary(g.Atomic(g.Terminal(g.String("a")))),
          g.Primary(g.Atomic(g.Terminal(g.String("*")))),
          g.Primary(g.Atomic(g.Terminal(g.String("+")))),
        ]),
      ),
    ])
}
