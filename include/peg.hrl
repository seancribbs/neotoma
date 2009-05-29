-define(root(Symbol),
        parse(Input) ->
  peg:setup_memo(?MODULE),
  Result = case peg:p(Input, Symbol, fun(I) -> Symbol(I) end) of
    {AST, []} -> AST;
    fail -> fail
  end,
  peg:release_memo(),
  Result).

-compile([{parse_transform, peg_transform}, export_all]).
