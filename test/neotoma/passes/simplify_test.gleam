import birdie
import neotoma/ir/norep as g
import neotoma/passes/simplify
import pprint

pub fn simplify_peephole_flatten_redundant_sequences_test() {
  let input =
    g.Grammar(
      name: "simplify_peephole_flatten_redundant_sequences_test",
      rules: [
        g.Definition(
          name: "top",
          expr: g.Sequence([
            g.Primary(g.Atomic(g.Terminal(g.String("neotoma")))),
          ]),
        ),
      ],
    )

  input
  |> simplify.simplify
  |> pprint.format
  |> birdie.snap(title: "simplify_peephole_flatten_redundant_sequences_test")
}

pub fn simplify_peephole_flatten_redundant_choices_test() {
  let input =
    g.Grammar(name: "simplify_peephole_flatten_redundant_choices_test", rules: [
      g.Definition(
        name: "top",
        expr: g.Choice([g.Primary(g.Atomic(g.Terminal(g.String("neotoma"))))]),
      ),
    ])

  input
  |> simplify.simplify
  |> pprint.format
  |> birdie.snap(title: "simplify_peephole_flatten_redundant_choices_test")
}

pub fn simplify_peephole_flatten_nested_redundant_sequences_test() {
  let input =
    g.Grammar(
      name: "simplify_peephole_flatten_nested_redundant_sequences_test",
      rules: [
        g.Definition(
          name: "top",
          expr: g.Primary(
            g.Optional(
              g.Sequence([g.Primary(g.Atomic(g.Terminal(g.String("neotoma"))))]),
            ),
          ),
        ),
      ],
    )

  input
  |> simplify.simplify
  |> pprint.format
  |> birdie.snap(
    title: "simplify_peephole_flatten_nested_redundant_sequences_test",
  )
}

pub fn simplify_peephole_flatten_nested_redundant_choices_test() {
  let input =
    g.Grammar(
      name: "simplify_peephole_flatten_nested_redundant_choices_test",
      rules: [
        g.Definition(
          name: "top",
          expr: g.Primary(
            g.Assert(
              g.Choice([g.Primary(g.Atomic(g.Terminal(g.String("neotoma"))))]),
            ),
          ),
        ),
      ],
    )

  input
  |> simplify.simplify
  |> pprint.format
  |> birdie.snap(
    title: "simplify_peephole_flatten_nested_redundant_choices_test",
  )
}

pub fn simplify_peephole_flatten_deep_nested_redundant_test() {
  let input =
    g.Grammar(
      name: "simplify_peephole_flatten_deep_nested_redundant_test",
      rules: [
        g.Definition(
          name: "top",
          expr: g.Choice([
            g.Sequence([
              g.Choice([
                g.Sequence([
                  g.Primary(g.Atomic(g.Terminal(g.String("neotoma")))),
                ]),
              ]),
            ]),
          ]),
        ),
      ],
    )

  input
  |> simplify.simplify
  |> pprint.format
  |> birdie.snap(title: "simplify_peephole_flatten_deep_nested_redundant_test")
}
