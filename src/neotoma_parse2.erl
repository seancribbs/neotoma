-module(neotoma_parse2).
-export([parse/1,file/1]).
-define(p_anything,true).
-define(p_charclass,true).
-define(p_choose,true).
-define(p_label,true).
-define(p_not,true).
-define(p_one_or_more,true).
-define(p_optional,true).
-define(p_scan,true).
-define(p_seq,true).
-define(p_string,true).
-define(p_zero_or_more,true).



-include("neotoma.hrl").

-spec file(file:name()) -> any().
file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  setup_memo(),
  Result = case 'rules'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

-spec 'rules'(input(), index()) -> parse_result().
'rules'(Input, Index) ->
  p(Input, Index, 'rules', fun(I,D) -> (p_seq([p_optional(fun 'space'/2), fun 'declaration_sequence'/2, p_optional(fun 'space'/2), p_optional(fun 'code_block'/2), p_optional(fun 'space'/2)]))(I,D) end, fun(Node, _Idx) ->
  [_, Decls, _, Code0, _] = Node,
  Code = case Code0 of
             #code{} -> Code0;
             _ -> undefined
         end,
  #grammar{declarations = Decls,
           code = Code}
 end).

-spec 'declaration_sequence'(input(), index()) -> parse_result().
'declaration_sequence'(Input, Index) ->
  p(Input, Index, 'declaration_sequence', fun(I,D) -> (p_seq([p_label('head', fun 'declaration'/2), p_label('tail', p_zero_or_more(p_seq([fun 'space'/2, fun 'declaration'/2])))]))(I,D) end, fun(Node, _Idx) ->
  FirstRule = proplists:get_value(head, Node),
  OtherRules =  [I || [_,I] <- proplists:get_value(tail, Node, [])],
  [FirstRule|OtherRules]
 end).

-spec 'declaration'(input(), index()) -> parse_result().
'declaration'(Input, Index) ->
  p(Input, Index, 'declaration', fun(I,D) -> (p_seq([fun 'nonterminal'/2, p_zero_or_more(fun 'space'/2), p_string(<<"<-">>), p_zero_or_more(fun 'space'/2), fun 'parsing_expression'/2, p_optional(fun 'space'/2), p_optional(fun 'code_block'/2), p_optional(fun 'space'/2), p_string(<<";">>)]))(I,D) end, fun(Node, Idx) ->
  [NT, _, _, _, Expr, _, Code0|_] = Node,
  Code = case Code0 of
      #code{} -> Code0;
      _ -> undefined
  end,
  #declaration{ name = NT#nonterminal.name,
                expr = Expr,
                code = Code,
                index = Idx }
 end).

-spec 'parsing_expression'(input(), index()) -> parse_result().
'parsing_expression'(Input, Index) ->
  p(Input, Index, 'parsing_expression', fun(I,D) -> (p_choose([fun 'choice'/2, fun 'sequence'/2, fun 'primary'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'choice'(input(), index()) -> parse_result().
'choice'(Input, Index) ->
  p(Input, Index, 'choice', fun(I,D) -> (p_seq([p_label('head', fun 'alternative'/2), p_label('tail', p_one_or_more(p_seq([fun 'space'/2, p_string(<<"\/">>), fun 'space'/2, fun 'alternative'/2])))]))(I,D) end, fun(Node, Idx) ->
  Tail = [lists:last(S) || S <- proplists:get_value(tail, Node)],
  Head = proplists:get_value(head, Node),
  #choice{alts = [Head|Tail],
          index = Idx}
 end).

-spec 'alternative'(input(), index()) -> parse_result().
'alternative'(Input, Index) ->
  p(Input, Index, 'alternative', fun(I,D) -> (p_choose([fun 'sequence'/2, fun 'labeled_primary'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'labeled_primary'(input(), index()) -> parse_result().
'labeled_primary'(Input, Index) ->
  p(Input, Index, 'labeled_primary', fun(I,D) -> (p_seq([p_optional(fun 'label'/2), fun 'primary'/2]))(I,D) end, fun(Node, Idx) ->
  case Node of
      [[], Primary] -> Primary;
      [Label, Primary] ->
          #label{expr=Primary, label=Label, index=Idx}
  end
 end).

-spec 'label'(input(), index()) -> parse_result().
'label'(Input, Index) ->
  p(Input, Index, 'label', fun(I,D) -> (p_seq([fun 'alpha_char'/2, p_zero_or_more(fun 'alphanumeric_char'/2), p_string(<<":">>)]))(I,D) end, fun(Node, _Idx) ->
  binary_to_atom(
    unicode:characters_to_binary(lists:sublist(Node, length(Node)-1)),
    utf8)
 end).

-spec 'primary'(input(), index()) -> parse_result().
'primary'(Input, Index) ->
  p(Input, Index, 'primary', fun(I,D) -> (p_choose([p_seq([fun 'prefix'/2, fun 'atomic'/2]), p_seq([fun 'atomic'/2, fun 'suffix'/2]), fun 'atomic'/2]))(I,D) end, fun(Node, Idx) ->
    case Node of
        [Atomic, plus] ->
            #plus{expr=Atomic, index=Idx};
        [Atomic, star] ->
            #star{expr=Atomic, index=Idx};
        [Atomic, optional] ->
            #optional{expr=Atomic, index=Idx};
        [assert, Atomic] ->
            #assert{expr=Atomic, index=Idx};
        [deny, Atomic] ->
            #deny{expr=Atomic, index=Idx};
        _ -> Node
    end
 end).

-spec 'sequence'(input(), index()) -> parse_result().
'sequence'(Input, Index) ->
  p(Input, Index, 'sequence', fun(I,D) -> (p_seq([p_label('head', fun 'labeled_primary'/2), p_label('tail', p_one_or_more(p_seq([fun 'space'/2, fun 'labeled_primary'/2])))]))(I,D) end, fun(Node, Idx) ->
  Tail = [lists:nth(2, S) || S <- proplists:get_value(tail, Node)],
  Head = proplists:get_value(head, Node),
  #sequence{exprs = [Head|Tail],
            index = Idx}
 end).

-spec 'suffix'(input(), index()) -> parse_result().
'suffix'(Input, Index) ->
  p(Input, Index, 'suffix', fun(I,D) -> (p_choose([fun 'repetition_suffix'/2, fun 'optional_suffix'/2]))(I,D) end, fun(Node, _Idx) ->
  case Node of
    <<"*">> -> star;
    <<"+">> -> plus;
    <<"?">> -> optional
  end
 end).

-spec 'optional_suffix'(input(), index()) -> parse_result().
'optional_suffix'(Input, Index) ->
  p(Input, Index, 'optional_suffix', fun(I,D) -> (p_string(<<"?">>))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'repetition_suffix'(input(), index()) -> parse_result().
'repetition_suffix'(Input, Index) ->
  p(Input, Index, 'repetition_suffix', fun(I,D) -> (p_choose([p_string(<<"+">>), p_string(<<"*">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'prefix'(input(), index()) -> parse_result().
'prefix'(Input, Index) ->
  p(Input, Index, 'prefix', fun(I,D) -> (p_choose([p_string(<<"&">>), p_string(<<"!">>)]))(I,D) end, fun(Node, _Idx) ->
  case Node of
    <<"&">> -> assert;
    <<"!">> -> deny
  end
 end).

-spec 'atomic'(input(), index()) -> parse_result().
'atomic'(Input, Index) ->
  p(Input, Index, 'atomic', fun(I,D) -> (p_choose([fun 'terminal'/2, fun 'nonterminal'/2, fun 'parenthesized_expression'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'parenthesized_expression'(input(), index()) -> parse_result().
'parenthesized_expression'(Input, Index) ->
  p(Input, Index, 'parenthesized_expression', fun(I,D) -> (p_seq([p_string(<<"(">>), p_optional(fun 'space'/2), fun 'parsing_expression'/2, p_optional(fun 'space'/2), p_string(<<")">>)]))(I,D) end, fun(Node, _Idx) ->
   lists:nth(3, Node)
 end).

-spec 'nonterminal'(input(), index()) -> parse_result().
'nonterminal'(Input, Index) ->
  p(Input, Index, 'nonterminal', fun(I,D) -> (p_seq([fun 'alpha_char'/2, p_zero_or_more(fun 'alphanumeric_char'/2)]))(I,D) end, fun(Node, Idx) ->
  Symbol = unicode:characters_to_binary(Node),
  #nonterminal{name = binary_to_atom(Symbol, utf8),
               index = Idx}
 end).

-spec 'terminal'(input(), index()) -> parse_result().
'terminal'(Input, Index) ->
  p(Input, Index, 'terminal', fun(I,D) -> (p_choose([fun 'regexp_string'/2, fun 'quoted_string'/2, fun 'character_class'/2, fun 'anything_symbol'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'regexp_string'(input(), index()) -> parse_result().
'regexp_string'(Input, Index) ->
  p(Input, Index, 'regexp_string', fun(I,D) -> (p_seq([p_string(<<"#">>), p_label('string', p_one_or_more(p_seq([p_not(p_string(<<"#">>)), p_choose([p_string(<<"\\#">>), p_anything()])]))), p_string(<<"#">>)]))(I,D) end, fun(Node, Idx) ->
  #regexp{regexp = unicode:characters_to_binary(proplists:get_value(string, Node)),
          index = Idx}
 end).

-spec 'quoted_string'(input(), index()) -> parse_result().
'quoted_string'(Input, Index) ->
  p(Input, Index, 'quoted_string', fun(I,D) -> (p_choose([fun 'single_quoted_string'/2, fun 'double_quoted_string'/2]))(I,D) end, fun(Node, Idx) ->
  #string{string = unicode:characters_to_binary(proplists:get_value(string, Node)),
          index = Idx}
 end).

-spec 'double_quoted_string'(input(), index()) -> parse_result().
'double_quoted_string'(Input, Index) ->
  p(Input, Index, 'double_quoted_string', fun(I,D) -> (p_seq([p_string(<<"\"">>), p_label('string', p_zero_or_more(p_seq([p_not(p_string(<<"\"">>)), p_choose([p_string(<<"\\\\">>), p_string(<<"\\\"">>), p_anything()])]))), p_string(<<"\"">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'single_quoted_string'(input(), index()) -> parse_result().
'single_quoted_string'(Input, Index) ->
  p(Input, Index, 'single_quoted_string', fun(I,D) -> (p_seq([p_string(<<"\'">>), p_label('string', p_zero_or_more(p_seq([p_not(p_string(<<"\'">>)), p_choose([p_string(<<"\\\\">>), p_string(<<"\\\'">>), p_anything()])]))), p_string(<<"\'">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'character_class'(input(), index()) -> parse_result().
'character_class'(Input, Index) ->
  p(Input, Index, 'character_class', fun(I,D) -> (p_seq([p_string(<<"[">>), p_label('characters', p_one_or_more(p_seq([p_not(p_string(<<"]">>)), p_choose([p_seq([p_string(<<"\\\\">>), p_anything()]), p_seq([p_not(p_string(<<"\\\\">>)), p_anything()])])]))), p_string(<<"]">>)]))(I,D) end, fun(Node, Idx) ->
  #charclass{
     charclass = unicode:characters_to_binary(proplists:get_value(characters, Node)),
     index = Idx
  }
 end).

-spec 'anything_symbol'(input(), index()) -> parse_result().
'anything_symbol'(Input, Index) ->
  p(Input, Index, 'anything_symbol', fun(I,D) -> (p_string(<<".">>))(I,D) end, fun(_Node, Idx) ->
  #anything{index=Idx}
 end).

-spec 'alpha_char'(input(), index()) -> parse_result().
'alpha_char'(Input, Index) ->
  p(Input, Index, 'alpha_char', fun(I,D) -> (p_charclass(<<"[A-Za-z_]">>))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'alphanumeric_char'(input(), index()) -> parse_result().
'alphanumeric_char'(Input, Index) ->
  p(Input, Index, 'alphanumeric_char', fun(I,D) -> (p_choose([fun 'alpha_char'/2, p_charclass(<<"[0-9]">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'space'(input(), index()) -> parse_result().
'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_one_or_more(p_choose([fun 'white'/2, fun 'comment_to_eol'/2])))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'comment_to_eol'(input(), index()) -> parse_result().
'comment_to_eol'(Input, Index) ->
  p(Input, Index, 'comment_to_eol', fun(I,D) -> (p_seq([p_not(p_string(<<"%{">>)), p_string(<<"%">>), p_zero_or_more(p_seq([p_not(p_string(<<"\n">>)), p_anything()]))]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'white'(input(), index()) -> parse_result().
'white'(Input, Index) ->
  p(Input, Index, 'white', fun(I,D) -> (p_charclass(<<"[\s\t\n\r]">>))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'code_block'(input(), index()) -> parse_result().
'code_block'(Input, Index) ->
  p(Input, Index, 'code_block', fun(I,D) -> (p_choose([p_seq([p_string(<<"%{">>), p_label('code', p_one_or_more(p_choose([p_string(<<"\\%">>), p_string(<<"$%">>), p_seq([p_not(p_string(<<"%}">>)), p_anything()])]))), p_string(<<"%}">>)]), p_seq([p_string(<<"`">>), p_label('code', p_one_or_more(p_choose([p_string(<<"\\`">>), p_string(<<"$`">>), p_seq([p_not(p_string(<<"`">>)), p_anything()])]))), p_string(<<"`">>)]), p_string(<<"~">>)]))(I,D) end, fun(Node, Idx) ->
   case Node of
       <<"~">> -> #code{identity=true, index=Idx};
       %% Note I use c2l here instead of c2b so that we can use
       %% erl_scan and friends. Erlang code should probably be latin1
       %% for safety.
       _   -> #code{code=unicode:characters_to_list(proplists:get_value('code', Node)),
                    index=Idx}
   end
 end).



-file("peg_includes.hrl", 1).
%% -type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec p(input(), index(), atom(), parse_fun(), xform_fun()) -> parse_result().
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

-spec setup_memo() -> ets:tid().
setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

-spec release_memo() -> true.
release_memo() ->
  ets:delete(memo_table_name()).

-spec memoize(index(), atom(), term()) -> true.
memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

-spec memo_table_name() -> ets:tid().
memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

-ifdef(p_eof).
-spec p_eof() -> parse_fun().
p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
-spec p_optional(parse_fun()) -> parse_fun().
p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.
-endif.

-ifdef(p_not).
-spec p_not(parse_fun()) -> parse_fun().
p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.
-endif.

-ifdef(p_assert).
-spec p_assert(parse_fun()) -> parse_fun().
p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.
-endif.

-ifdef(p_seq).
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

-spec p_all([parse_fun()], input(), index(), [term()]) -> parse_result().
p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.
-endif.

-ifdef(p_choose).
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

-spec p_attempt([parse_fun()], input(), index(), none | parse_failure()) -> parse_result().
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
-endif.

-ifdef(p_zero_or_more).
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.
-endif.

-ifdef(p_one_or_more).
-spec p_one_or_more(parse_fun()) -> parse_fun().
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
-endif.

-ifdef(p_label).
-spec p_label(atom(), parse_fun()) -> parse_fun().
p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.
-endif.

-ifdef(p_scan).
-spec p_scan(parse_fun(), input(), index(), [term()]) -> parse_result().
p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.
-endif.

-ifdef(p_string).
-spec p_string(binary()) -> parse_fun().
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.
-endif.

-ifdef(p_anything).
-spec p_anything() -> parse_fun().
p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.
-endif.

-ifdef(p_charclass).
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.
-endif.

-ifdef(p_regexp).
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
    {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ -> {fail, {expected, {regexp, binary_to_list(Regexp)}, Index}}
        end
    end.
-endif.

-ifdef(line).
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line,L},_}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
-spec column(index() | term()) -> pos_integer() | undefined.
column({_,{column,C}}) -> C;
column(_) -> undefined.
-endif.

-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
