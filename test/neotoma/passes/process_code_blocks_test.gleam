import birdie
import gleam/option
import neotoma/code
import neotoma/ir/grammar as g
import neotoma/passes/expand_charclasses
import neotoma/passes/expand_repetition
import neotoma/passes/process_code_blocks
import neotoma/passes/prohibit_indirect_lr
import neotoma/passes/rewrite_simple_lr
import neotoma/passes/simplify
import pprint

pub fn process_code_blocks_parses_top_level_code_test() {
  let title = "process_code_blocks_parses_top_level_code_test"
  let assert Ok(out) = g.Grammar(
    name: title,
    rules: [
      g.Definition(
        name: "start",
        expr: g.Sequence(
          [
            g.Primary(g.Labeled(
              g.Primary(
                g.OneOrMore(g.Primary(g.Atomic(g.Terminal(g.Anything)))),
              ),
              "String",
            )),
          ],
          option.Some(code.Unparsed("unescape(String)")),
        ),
      ),
    ],
    top_level_code: option.Some(
      "unescape(Input) -> string:replace(unicode:characters_to_binary(Input), <<\"..\">>, <<\".\">>, all).",
    ),
  )
  |> pre_passes
  |> process_code_blocks.process_code_blocks

  out
  |> pprint.format
  |> birdie.snap(title:)
}

pub fn process_code_blocks_parses_inline_code_test() {
  let title = "process_code_blocks_parses_inline_code_test"
  let assert Ok(out) = g.Grammar(
    name: title,
    rules: [
      g.Definition(
        name: "start",
        expr: g.Sequence(
          [
            g.Primary(g.Atomic(g.Terminal(g.String("(")))),
            g.Primary(g.Labeled(
              g.Primary(
                g.OneOrMore(g.Primary(g.Atomic(g.Nonterminal("inner")))),
              ),
              "Inner",
            )),
            g.Primary(g.Atomic(g.Terminal(g.String(")")))),
          ],
          option.Some(code.Unparsed("Inner")),
        ),
      ),
      g.Definition(
        name: "inner",
        expr: g.Primary(g.Atomic(g.Terminal(g.Anything))),
      ),
    ],
    top_level_code: option.None,
  )
  |> pre_passes
  |> process_code_blocks.process_code_blocks()

  out
  |> pprint.format
  |> birdie.snap(title:)
}

fn pre_passes(g: g.Grammar) {
  let assert Ok(lr) =
    g
    |> expand_charclasses.expand_charclasses
    |> expand_repetition.expand_repetition
    |> rewrite_simple_lr.rewrite_simple_lr
    |> prohibit_indirect_lr.prohibit_indirect_lr

  simplify.simplify(lr)
}
