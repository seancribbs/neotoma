import gleam/io
import neotoma/grammar as g
import neotoma/passes/concrete_erlang
import neotoma/passes/expand_charclasses
import neotoma/passes/generate_abstract
import neotoma/syntax

pub fn main() -> Nil {
  let _ =
    g.Grammar(name: "generate_abstract_assert_test", rules: [
      g.Definition(
        name: "start",
        expr: g.Sequence([
          g.Primary(g.Assert(g.Primary(g.Atomic(g.Terminal(g.String("neo")))))),
          g.Choice([
            g.Primary(g.Atomic(g.Terminal(g.String("neotoma")))),
            g.Primary(g.Atomic(g.Terminal(g.String("neon")))),
          ]),
        ]),
      ),
    ])
    |> expand_charclasses.expand_charclasses
    |> generate_abstract.generate_abstract_module
    |> concrete_erlang.lower
    |> syntax.format
    |> io.println
}
