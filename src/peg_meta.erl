-module(peg_meta).
-export([parse/1,file/1]).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1]}).

file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(binary_to_list(Bin)).

parse(Input) ->
  setup_memo('peg_meta'),
  Result = case 'rules'(Input,{{line,1},{column,1}}) of
             {AST, [], _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

'rules'(Input, Index) ->
  p(Input, Index, 'rules', fun(I,D) -> (p_seq([p_optional(fun 'space'/2), fun 'declaration_sequence'/2, p_optional(fun 'space'/2)]))(I,D) end, fun(Node, Idx) -> transform('rules', Node, Idx) end).

'declaration_sequence'(Input, Index) ->
  p(Input, Index, 'declaration_sequence', fun(I,D) -> (p_seq([p_label('head', fun 'declaration'/2), p_label('tail', p_zero_or_more(p_seq([fun 'space'/2, fun 'declaration'/2])))]))(I,D) end, fun(Node, Idx) -> transform('declaration_sequence', Node, Idx) end).

'declaration'(Input, Index) ->
  p(Input, Index, 'declaration', fun(I,D) -> (p_seq([fun 'nonterminal'/2, fun 'space'/2, p_string("<-"), fun 'space'/2, fun 'parsing_expression'/2, p_optional(fun 'space'/2), p_string(";")]))(I,D) end, fun(Node, Idx) -> transform('declaration', Node, Idx) end).

'parsing_expression'(Input, Index) ->
  p(Input, Index, 'parsing_expression', fun(I,D) -> (p_choose([fun 'choice'/2, fun 'sequence'/2, fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> transform('parsing_expression', Node, Idx) end).

'choice'(Input, Index) ->
  p(Input, Index, 'choice', fun(I,D) -> (p_seq([p_label('head', fun 'alternative'/2), p_label('tail', p_one_or_more(p_seq([fun 'space'/2, p_string("/"), fun 'space'/2, fun 'alternative'/2])))]))(I,D) end, fun(Node, Idx) -> transform('choice', Node, Idx) end).

'alternative'(Input, Index) ->
  p(Input, Index, 'alternative', fun(I,D) -> (p_choose([fun 'sequence'/2, fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> transform('alternative', Node, Idx) end).

'primary'(Input, Index) ->
  p(Input, Index, 'primary', fun(I,D) -> (p_choose([p_seq([fun 'prefix'/2, fun 'atomic'/2]), p_seq([fun 'atomic'/2, fun 'suffix'/2]), fun 'atomic'/2]))(I,D) end, fun(Node, Idx) -> transform('primary', Node, Idx) end).

'sequence'(Input, Index) ->
  p(Input, Index, 'sequence', fun(I,D) -> (p_seq([p_label('head', fun 'labeled_sequence_primary'/2), p_label('tail', p_one_or_more(p_seq([fun 'space'/2, fun 'labeled_sequence_primary'/2])))]))(I,D) end, fun(Node, Idx) -> transform('sequence', Node, Idx) end).

'labeled_sequence_primary'(Input, Index) ->
  p(Input, Index, 'labeled_sequence_primary', fun(I,D) -> (p_seq([p_optional(fun 'label'/2), fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> transform('labeled_sequence_primary', Node, Idx) end).

'label'(Input, Index) ->
  p(Input, Index, 'label', fun(I,D) -> (p_seq([fun 'alpha_char'/2, p_zero_or_more(fun 'alphanumeric_char'/2), p_string(":")]))(I,D) end, fun(Node, Idx) -> transform('label', Node, Idx) end).

'suffix'(Input, Index) ->
  p(Input, Index, 'suffix', fun(I,D) -> (p_choose([fun 'repetition_suffix'/2, fun 'optional_suffix'/2]))(I,D) end, fun(Node, Idx) -> transform('suffix', Node, Idx) end).

'optional_suffix'(Input, Index) ->
  p(Input, Index, 'optional_suffix', fun(I,D) -> (p_string("?"))(I,D) end, fun(Node, Idx) -> transform('optional_suffix', Node, Idx) end).

'repetition_suffix'(Input, Index) ->
  p(Input, Index, 'repetition_suffix', fun(I,D) -> (p_choose([p_string("+"), p_string("*")]))(I,D) end, fun(Node, Idx) -> transform('repetition_suffix', Node, Idx) end).

'prefix'(Input, Index) ->
  p(Input, Index, 'prefix', fun(I,D) -> (p_choose([p_string("&"), p_string("!")]))(I,D) end, fun(Node, Idx) -> transform('prefix', Node, Idx) end).

'atomic'(Input, Index) ->
  p(Input, Index, 'atomic', fun(I,D) -> (p_choose([fun 'terminal'/2, fun 'nonterminal'/2, fun 'parenthesized_expression'/2]))(I,D) end, fun(Node, Idx) -> transform('atomic', Node, Idx) end).

'parenthesized_expression'(Input, Index) ->
  p(Input, Index, 'parenthesized_expression', fun(I,D) -> (p_seq([p_string("("), p_optional(fun 'space'/2), fun 'parsing_expression'/2, p_optional(fun 'space'/2), p_string(")")]))(I,D) end, fun(Node, Idx) -> transform('parenthesized_expression', Node, Idx) end).

'nonterminal'(Input, Index) ->
  p(Input, Index, 'nonterminal', fun(I,D) -> (p_seq([fun 'alpha_char'/2, p_zero_or_more(fun 'alphanumeric_char'/2)]))(I,D) end, fun(Node, Idx) -> transform('nonterminal', Node, Idx) end).

'terminal'(Input, Index) ->
  p(Input, Index, 'terminal', fun(I,D) -> (p_choose([fun 'quoted_string'/2, fun 'character_class'/2, fun 'anything_symbol'/2]))(I,D) end, fun(Node, Idx) -> transform('terminal', Node, Idx) end).

'quoted_string'(Input, Index) ->
  p(Input, Index, 'quoted_string', fun(I,D) -> (p_choose([fun 'single_quoted_string'/2, fun 'double_quoted_string'/2]))(I,D) end, fun(Node, Idx) -> transform('quoted_string', Node, Idx) end).

'double_quoted_string'(Input, Index) ->
  p(Input, Index, 'double_quoted_string', fun(I,D) -> (p_seq([p_string("\""), p_label('string', p_zero_or_more(p_seq([p_not(p_string("\"")), p_choose([p_string("\\\\"), p_string("\\\""), p_anything()])]))), p_string("\"")]))(I,D) end, fun(Node, Idx) -> transform('double_quoted_string', Node, Idx) end).

'single_quoted_string'(Input, Index) ->
  p(Input, Index, 'single_quoted_string', fun(I,D) -> (p_seq([p_string("'"), p_label('string', p_zero_or_more(p_seq([p_not(p_string("'")), p_choose([p_string("\\\\"), p_string("\\'"), p_anything()])]))), p_string("'")]))(I,D) end, fun(Node, Idx) -> transform('single_quoted_string', Node, Idx) end).

'character_class'(Input, Index) ->
  p(Input, Index, 'character_class', fun(I,D) -> (p_seq([p_string("["), p_label('characters', p_one_or_more(p_seq([p_not(p_string("]")), p_choose([p_seq([p_string("\\\\"), p_anything()]), p_seq([p_not(p_string("\\\\")), p_anything()])])]))), p_string("]")]))(I,D) end, fun(Node, Idx) -> transform('character_class', Node, Idx) end).

'anything_symbol'(Input, Index) ->
  p(Input, Index, 'anything_symbol', fun(I,D) -> (p_string("."))(I,D) end, fun(Node, Idx) -> transform('anything_symbol', Node, Idx) end).

'alpha_char'(Input, Index) ->
  p(Input, Index, 'alpha_char', fun(I,D) -> (p_charclass("[a-z_]"))(I,D) end, fun(Node, Idx) -> transform('alpha_char', Node, Idx) end).

'alphanumeric_char'(Input, Index) ->
  p(Input, Index, 'alphanumeric_char', fun(I,D) -> (p_choose([fun 'alpha_char'/2, p_charclass("[0-9]")]))(I,D) end, fun(Node, Idx) -> transform('alphanumeric_char', Node, Idx) end).

'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_one_or_more(p_choose([fun 'white'/2, fun 'comment_to_eol'/2])))(I,D) end, fun(Node, Idx) -> transform('space', Node, Idx) end).

'comment_to_eol'(Input, Index) ->
  p(Input, Index, 'comment_to_eol', fun(I,D) -> (p_seq([p_string("%"), p_zero_or_more(p_seq([p_not(p_string("\n")), p_anything()]))]))(I,D) end, fun(Node, Idx) -> transform('comment_to_eol', Node, Idx) end).

'white'(Input, Index) ->
  p(Input, Index, 'white', fun(I,D) -> (p_charclass("[ \t\n\r]"))(I,D) end, fun(Node, Idx) -> transform('white', Node, Idx) end).

transform(Symbol,Node,Index) -> peg_meta_gen:transform(Symbol, Node, Index).





p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  % Grab the memo table from ets
  Memo = get_memo(StartIndex),
  % See if the current reduction is memoized
  case dict:find(Name, Memo) of
    % If it is, return the result
    {ok, Result} -> Result;
    % If not, attempt to parse
    _ ->
      case ParseFun(Inp, StartIndex) of
        % If it fails, memoize the failure
        {fail,_} = Failure ->
          memoize(StartIndex, dict:store(Name, Failure, Memo)),
          Failure;
        % If it passes, transform and memoize the result.
        {Result, InpRem, NewIndex} ->
          Transformed = TransformFun(Result, StartIndex),
          memoize(StartIndex, dict:store(Name, {Transformed, InpRem, NewIndex}, Memo)),
          {Transformed, InpRem, NewIndex}
      end
  end.

setup_memo(Name) ->
  TID = ets:new(Name, [set]),
  put(ets_table, TID).

release_memo() ->
  ets:delete(get(ets_table)),
  erase(ets_table).

memoize(Position, Struct) ->
  ets:insert(get(ets_table), {Position, Struct}).

get_memo(Position) ->
  case ets:lookup(get(ets_table), Position) of
    [] -> dict:new();
    [{Position, Dict}] -> Dict
  end.

p_eof() ->
  fun([], Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.

p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.

p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.

p_and(P) ->
  p_seq(P).

p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.

p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.

p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.

p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.

p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.

p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.

p_string(S) ->
  fun(Input, Index) ->
      case lists:prefix(S, Input) of
        true -> {S, lists:sublist(Input, length(S)+1, length(Input)), p_advance_index(S,Index)};
        _ -> {fail, {expected, {string, S}, Index}}
      end
  end.

p_anything() ->
  fun([], Index) -> {fail, {expected, any_character, Index}};
     ([H|T], Index) -> {H, T, p_advance_index(H, Index)}
  end.

p_charclass(Class) ->
  fun(Inp, Index) ->
     {ok, RE} = re:compile("^"++Class),
      case re:run(Inp, RE) of
        {match, _} ->
          {hd(Inp), tl(Inp), p_advance_index(hd(Inp), Index)};
        _ -> {fail,{expected, {character_class, Class}, Index}}
      end
  end.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) -> % strings
  lists:foldl(fun p_advance_index/2, Index, MatchedInput);
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
