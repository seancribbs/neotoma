import neotoma/syntax

pub fn abstract_printing_test() {
  let pp = "neotoma" |> syntax.abstract() |> syntax.format()

  assert pp == "<<110, 101, 111, 116, 111, 109, 97>>"
  // Pretty printer prints the individual codepoints of each character
}
