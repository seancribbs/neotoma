import neotoma/ir/norep as g

pub fn norep_size_test() {
  let str_foo = g.Primary(g.Atomic(g.Terminal(g.String("foo"))))
  assert g.size(str_foo) == 1
  assert g.size(g.Primary(g.Atomic(g.Nonterminal("foo")))) == 1
  assert g.size(g.Primary(g.Assert(str_foo))) == 1
  assert g.size(g.Primary(g.Deny(str_foo))) == 1
  assert g.size(g.Primary(g.Optional(str_foo))) == 2
  assert g.size(g.Sequence([str_foo, str_foo])) == 3
  assert g.size(g.Choice([str_foo, str_foo])) == 3
}
