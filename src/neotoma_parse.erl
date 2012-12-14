-module(neotoma_parse).
-export([parse/1, file/1]).
-compile(nowarn_unused_vars).

-export([
    'code_block'/2
    , 'white'/2
    , 'comment_to_eol'/2
    , 'space'/2
    , 'alphanumeric_char'/2
    , 'alpha_char'/2
    , 'anything_symbol'/2
    , 'character_class'/2
    , 'single_quoted_string'/2
    , 'double_quoted_string'/2
    , 'quoted_string'/2
    , 'terminal'/2
    , 'atom'/2
    , 'nonterminal'/2
    , 'parenthesized_expression'/2
    , 'atomic'/2
    , 'prefix'/2
    , 'suffix'/2
    , 'label'/2
    , 'labeled_primary'/2
    , 'sequence'/2
    , 'primary'/2
    , 'alternative'/2
    , 'choice'/2
    , 'parsing_expression'/2
    , 'declaration'/2
    , 'declaration_sequence'/2
    , 'rules'/2]).

% insert escapes into a string
escape_string(String) ->
	escape_string(String, []).

escape_string("", Output) ->
	lists:reverse(Output);
escape_string("/" ++ T, Output) ->
	escape_string(T, [$/, $\\ | Output]);
escape_string("\"" ++ T, Output) ->
	escape_string(T, [$", $\\ | Output]);
escape_string("'" ++ T, Output) ->
	escape_string(T, [$', $\\ | Output]);
escape_string("\b" ++ T, Output) ->
	escape_string(T, [$b, $\\ | Output]);
escape_string("\d" ++ T, Output) ->
	escape_string(T, [$d, $\\ | Output]);
escape_string("\e" ++ T, Output) ->
	escape_string(T, [$e, $\\ | Output]);
escape_string("\f" ++ T, Output) ->
	escape_string(T, [$f, $\\ | Output]);
escape_string("\n" ++ T, Output) ->
	escape_string(T, [$n, $\\ | Output]);
escape_string("\r" ++ T, Output) ->
	escape_string(T, [$r, $\\ | Output]);
escape_string("\s" ++ T, Output) ->
	escape_string(T, [$s, $\\ | Output]);
escape_string("\t" ++ T, Output) ->
	escape_string(T, [$t, $\\ | Output]);
escape_string("\v" ++ T, Output) ->
	escape_string(T, [$v, $\\ | Output]);
escape_string([C | T], Output) ->
	escape_string(T, [C | Output]).

add_lhs(Symbol, Index) ->
	TableName = neotoma_util:memo_table_name(),
	case ets:lookup(TableName, lhs)
		of [] ->
			ets:insert(TableName, {lhs, [{Symbol, Index}]})
		; [{lhs, L}] when is_list(L) ->
			ets:insert(TableName, {lhs, [{Symbol, Index} | L]})
	end.

add_nt(Symbol, Index) ->
	TableName = neotoma_util:memo_table_name(),
	case ets:lookup(TableName, nts)
		of [] ->
			ets:insert(TableName, {nts, [{Symbol, Index}]})
		; [{nts, L}] when is_list(L) ->
			case proplists:is_defined(Symbol, L)
				of true ->
					ok
				; _ ->
					ets:insert(TableName, {nts, [{Symbol, Index} | L]})
			end
	end.

verify_rules() ->
	TableName = neotoma_util:memo_table_name(),
	[{lhs, LHS}] = ets:lookup(TableName, lhs),
	[{nts, NTs}] = ets:lookup(TableName, nts),
	[Root | NonRoots] = lists:reverse(LHS),
	lists:foreach(fun({Sym, Idx}) ->
		case proplists:is_defined(Sym, NTs)
			of true ->
				ok
			; _ ->
				io:format("neotoma warning: rule '~s' is unused. ~p~n", [Sym,Idx])
			end
		end, NonRoots),
	lists:foreach(fun({Sym, Idx}) ->
		case proplists:is_defined(Sym, LHS)
			of true ->
				ok
			; _ ->
				io:format("neotoma error: nonterminal '~s' has no reduction. (found at ~p) No parser will be generated!~n", [Sym, Idx]),
				exit({neotoma, {no_reduction, Sym}})
			end
		end, NTs),
	Root.

-spec file(file:name()) -> any().
file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(Bin).

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  neotoma_util:setup_memo(),
  Result = case 'rules'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  neotoma_util:release_memo(), Result.

'rules'(Input, Index) ->
  neotoma_util:p(Input, Index, 'rules', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_optional(fun 'space'/2), fun 'declaration_sequence'/2, neotoma_util:p_optional(fun 'space'/2), neotoma_util:p_optional(fun 'code_block'/2), neotoma_util:p_optional(fun 'space'/2)]))(I,D) end, fun(Node, Idx) -> 
  RootRule = verify_rules(),
  Rules = iolist_to_binary(lists:map(fun(R) -> [R, "\n\n"] end, lists:nth(2, Node))),
  Code = case lists:nth(4, Node) of
             {code, Block} -> Block;
             _ -> []
         end,
  [{lhs, LHS}] = ets:lookup(neotoma_util:memo_table_name(), lhs),
  [{rules, Rules},
   {code, Code},
   {root, RootRule},
   {all_rules, LHS},
   {transform, ets:lookup(neotoma_util:memo_table_name(),gen_transform)}]
 end).

'declaration_sequence'(Input, Index) ->
  neotoma_util:p(Input, Index, 'declaration_sequence', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_label('head', fun 'declaration'/2), neotoma_util:p_label('tail', neotoma_util:p_zero_or_more(neotoma_util:p_seq([fun 'space'/2, fun 'declaration'/2])))]))(I,D) end, fun(Node, Idx) -> 
  FirstRule = proplists:get_value(head, Node),
  OtherRules =  [I || [_,I] <- proplists:get_value(tail, Node, [])],
  [FirstRule|OtherRules]
 end).

'declaration'(Input, Index) ->
  neotoma_util:p(Input, Index, 'declaration', fun(I,D) -> (neotoma_util:p_seq([fun 'atom'/2, fun 'space'/2, neotoma_util:p_string(<<"<-">>), fun 'space'/2, fun 'parsing_expression'/2, neotoma_util:p_optional(fun 'space'/2), neotoma_util:p_optional(fun 'code_block'/2), neotoma_util:p_optional(fun 'space'/2), neotoma_util:p_string(<<";">>)]))(I,D) end, fun(Node, Idx) -> 
  [Symbol | Tail] = Node,
  add_lhs(Symbol, Index),
  Transform = case lists:nth(6,Tail) of
                  {code, CodeBlock} -> CodeBlock;
                  _ ->
                      ets:insert_new(neotoma_util:memo_table_name(),{gen_transform, true}),
                      ["transform('",Symbol,"', Node, Idx)"]
                  end,
  ["'",Symbol,"'","(Input, Index) ->\n  ",
        "neotoma_util:p(Input, Index, '",Symbol,"', fun(I,D) -> (",
        lists:nth(4, Tail),
        ")(I,D) end, fun(Node, Idx) -> ",Transform," end)."]
 end).

'parsing_expression'(Input, Index) ->
  neotoma_util:p(Input, Index, 'parsing_expression', fun(I,D) -> (neotoma_util:p_choose([fun 'choice'/2, fun 'sequence'/2, fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'choice'(Input, Index) ->
  neotoma_util:p(Input, Index, 'choice', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_label('head', fun 'alternative'/2), neotoma_util:p_label('tail', neotoma_util:p_one_or_more(neotoma_util:p_seq([fun 'space'/2, neotoma_util:p_string(<<"\/">>), fun 'space'/2, fun 'alternative'/2])))]))(I,D) end, fun(Node, Idx) -> 
  Tail = [[", ", lists:last(S)] || S <- proplists:get_value(tail, Node)],
  Head = proplists:get_value(head, Node),
  ["neotoma_util:p_choose([", Head, Tail, "])"]
 end).

'alternative'(Input, Index) ->
  neotoma_util:p(Input, Index, 'alternative', fun(I,D) -> (neotoma_util:p_choose([fun 'sequence'/2, fun 'labeled_primary'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'primary'(Input, Index) ->
  neotoma_util:p(Input, Index, 'primary', fun(I,D) -> (neotoma_util:p_choose([neotoma_util:p_seq([fun 'prefix'/2, fun 'atomic'/2]), neotoma_util:p_seq([fun 'atomic'/2, fun 'suffix'/2]), fun 'atomic'/2]))(I,D) end, fun(Node, Idx) -> 
case Node of
  [Atomic, one_or_more] -> ["neotoma_util:p_one_or_more(", Atomic, ")"];
  [Atomic, zero_or_more] -> ["neotoma_util:p_zero_or_more(", Atomic, ")"];
  [Atomic, optional] -> ["neotoma_util:p_optional(", Atomic, ")"];
  [assert, Atomic] -> ["neotoma_util:p_assert(", Atomic, ")"];
  [not_, Atomic] -> ["neotoma_util:p_not(", Atomic, ")"];
  _ -> Node
end
 end).

'sequence'(Input, Index) ->
  neotoma_util:p(Input, Index, 'sequence', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_label('head', fun 'labeled_primary'/2), neotoma_util:p_label('tail', neotoma_util:p_one_or_more(neotoma_util:p_seq([fun 'space'/2, fun 'labeled_primary'/2])))]))(I,D) end, fun(Node, Idx) -> 
  Tail = [lists:nth(2, S) || S <- proplists:get_value(tail, Node)],
  Head = proplists:get_value(head, Node),
  Statements = [[", ", TS] || TS <- Tail],
  ["neotoma_util:p_seq([", Head, Statements, "])"]
 end).

'labeled_primary'(Input, Index) ->
  neotoma_util:p(Input, Index, 'labeled_primary', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_optional(fun 'label'/2), fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> 
  case hd(Node) of
    [] -> lists:nth(2, Node);
    Label -> ["neotoma_util:p_label('",  Label, "', ", lists:nth(2, Node), ")"]
  end
 end).

'label'(Input, Index) ->
  neotoma_util:p(Input, Index, 'label', fun(I,D) -> (neotoma_util:p_seq([fun 'alpha_char'/2, neotoma_util:p_zero_or_more(fun 'alphanumeric_char'/2), neotoma_util:p_string(<<":">>)]))(I,D) end, fun(Node, Idx) -> 
lists:sublist(Node, 1, length(Node) - 1)
 end).

'suffix'(Input, Index) ->
  neotoma_util:p(Input, Index, 'suffix', fun(I,D) -> (neotoma_util:p_choose([neotoma_util:p_string(<<"+">>), neotoma_util:p_string(<<"*">>), neotoma_util:p_string(<<"?">>)]))(I,D) end, fun(Node, Idx) -> 
  case Node of
    <<"*">> -> zero_or_more;
    <<"+">> -> one_or_more;
    <<"?">> -> optional
  end
 end).

'prefix'(Input, Index) ->
  neotoma_util:p(Input, Index, 'prefix', fun(I,D) -> (neotoma_util:p_choose([neotoma_util:p_string(<<"&">>), neotoma_util:p_string(<<"!">>)]))(I,D) end, fun(Node, Idx) -> 
case Node
	of <<"&">> -> assert
	; <<"!">> -> not_
end
 end).

'atomic'(Input, Index) ->
  neotoma_util:p(Input, Index, 'atomic', fun(I,D) -> (neotoma_util:p_choose([fun 'terminal'/2, fun 'nonterminal'/2, fun 'parenthesized_expression'/2]))(I,D) end, fun(Node, Idx) -> 
case Node
	of {nonterminal, Symbol} ->
		[<<"fun '">>, Symbol, <<"'/2">>]
	; {nonterminal, Module, Symbol} ->
		[<<"fun '">>, Module, <<"':'">>, Symbol, <<"'/2">>]
	; _ ->
		Node
end
 end).

'parenthesized_expression'(Input, Index) ->
  neotoma_util:p(Input, Index, 'parenthesized_expression', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_string(<<"(">>), neotoma_util:p_optional(fun 'space'/2), fun 'parsing_expression'/2, neotoma_util:p_optional(fun 'space'/2), neotoma_util:p_string(<<")">>)]))(I,D) end, fun(Node, Idx) -> 
lists:nth(3, Node)
 end).

'nonterminal'(Input, Index) ->
  neotoma_util:p(Input, Index, 'nonterminal', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_optional(neotoma_util:p_seq([neotoma_util:p_string(<<":">>), fun 'atom'/2, neotoma_util:p_string(<<":">>)])), fun 'atom'/2]))(I,D) end, fun(Node, Idx) -> 
case Node
	of [[_, Module, _], Symbol] ->
		{nonterminal, Module, Symbol}
	; [[], Symbol] ->
		add_nt(Symbol, Idx),
		{nonterminal, Symbol}
end
 end).

'atom'(Input, Index) ->
  neotoma_util:p(Input, Index, 'atom', fun(I,D) -> (neotoma_util:p_seq([fun 'alpha_char'/2, neotoma_util:p_zero_or_more(fun 'alphanumeric_char'/2)]))(I,D) end, fun(Node, Idx) -> iolist_to_binary(Node) end).

'terminal'(Input, Index) ->
  neotoma_util:p(Input, Index, 'terminal', fun(I,D) -> (neotoma_util:p_choose([fun 'quoted_string'/2, fun 'character_class'/2, fun 'anything_symbol'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'quoted_string'(Input, Index) ->
  neotoma_util:p(Input, Index, 'quoted_string', fun(I,D) -> (neotoma_util:p_choose([fun 'single_quoted_string'/2, fun 'double_quoted_string'/2]))(I,D) end, fun(Node, Idx) -> 
  lists:flatten(["neotoma_util:p_string(<<\"",
   escape_string(binary_to_list(iolist_to_binary(proplists:get_value(string, Node)))),
   "\">>)"])
 end).

'double_quoted_string'(Input, Index) ->
  neotoma_util:p(Input, Index, 'double_quoted_string', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_string(<<"\"">>), neotoma_util:p_label('string', neotoma_util:p_zero_or_more(neotoma_util:p_seq([neotoma_util:p_not(neotoma_util:p_string(<<"\"">>)), neotoma_util:p_choose([neotoma_util:p_string(<<"\\\\">>), neotoma_util:p_string(<<"\\\"">>), neotoma_util:p_anything()])]))), neotoma_util:p_string(<<"\"">>)]))(I,D) end, fun(Node, Idx) -> Node end).

'single_quoted_string'(Input, Index) ->
  neotoma_util:p(Input, Index, 'single_quoted_string', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_string(<<"\'">>), neotoma_util:p_label('string', neotoma_util:p_zero_or_more(neotoma_util:p_seq([neotoma_util:p_not(neotoma_util:p_string(<<"\'">>)), neotoma_util:p_choose([neotoma_util:p_string(<<"\\\\">>), neotoma_util:p_string(<<"\\\'">>), neotoma_util:p_anything()])]))), neotoma_util:p_string(<<"\'">>)]))(I,D) end, fun(Node, Idx) -> Node end).

'character_class'(Input, Index) ->
  neotoma_util:p(Input, Index, 'character_class', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_string(<<"[">>), neotoma_util:p_label('characters', neotoma_util:p_one_or_more(neotoma_util:p_seq([neotoma_util:p_not(neotoma_util:p_string(<<"]">>)), neotoma_util:p_choose([neotoma_util:p_seq([neotoma_util:p_string(<<"\\\\">>), neotoma_util:p_anything()]), neotoma_util:p_seq([neotoma_util:p_not(neotoma_util:p_string(<<"\\\\">>)), neotoma_util:p_anything()])])]))), neotoma_util:p_string(<<"]">>)]))(I,D) end, fun(Node, Idx) -> 
["neotoma_util:p_charclass(<<\"[",
   escape_string(binary_to_list(iolist_to_binary(proplists:get_value(characters, Node)))),
 "]\">>)"]
 end).

'anything_symbol'(Input, Index) ->
  neotoma_util:p(Input, Index, 'anything_symbol', fun(I,D) -> (neotoma_util:p_string(<<".">>))(I,D) end, fun(Node, Idx) ->  <<"neotoma_util:p_anything()">>  end).

'alpha_char'(Input, Index) ->
  neotoma_util:p(Input, Index, 'alpha_char', fun(I,D) -> (neotoma_util:p_charclass(<<"[A-Za-z_]">>))(I,D) end, fun(Node, Idx) -> Node end).

'alphanumeric_char'(Input, Index) ->
  neotoma_util:p(Input, Index, 'alphanumeric_char', fun(I,D) -> (neotoma_util:p_charclass(<<"[A-Za-z_0-9]">>))(I,D) end, fun(Node, Idx) -> Node end).

'space'(Input, Index) ->
  neotoma_util:p(Input, Index, 'space', fun(I,D) -> (neotoma_util:p_one_or_more(neotoma_util:p_choose([fun 'white'/2, fun 'comment_to_eol'/2])))(I,D) end, fun(Node, Idx) -> Node end).

'comment_to_eol'(Input, Index) ->
  neotoma_util:p(Input, Index, 'comment_to_eol', fun(I,D) -> (neotoma_util:p_seq([neotoma_util:p_not(neotoma_util:p_string(<<"%{">>)), neotoma_util:p_string(<<"%">>), neotoma_util:p_zero_or_more(neotoma_util:p_seq([neotoma_util:p_not(neotoma_util:p_string(<<"\n">>)), neotoma_util:p_anything()]))]))(I,D) end, fun(Node, Idx) -> Node end).

'white'(Input, Index) ->
  neotoma_util:p(Input, Index, 'white', fun(I,D) -> (neotoma_util:p_charclass(<<"[\s\t\n\r]">>))(I,D) end, fun(Node, Idx) -> Node end).

'code_block'(Input, Index) ->
  neotoma_util:p(Input, Index, 'code_block', fun(I,D) -> (neotoma_util:p_choose([neotoma_util:p_seq([neotoma_util:p_string(<<"%{">>), neotoma_util:p_label('code', neotoma_util:p_one_or_more(neotoma_util:p_choose([neotoma_util:p_string(<<"\\%">>), neotoma_util:p_string(<<"$%">>), neotoma_util:p_seq([neotoma_util:p_not(neotoma_util:p_string(<<"%}">>)), neotoma_util:p_anything()])]))), neotoma_util:p_string(<<"%}">>)]), neotoma_util:p_seq([neotoma_util:p_string(<<"`">>), neotoma_util:p_label('code', neotoma_util:p_one_or_more(neotoma_util:p_choose([neotoma_util:p_string(<<"\\`">>), neotoma_util:p_string(<<"$`">>), neotoma_util:p_seq([neotoma_util:p_not(neotoma_util:p_string(<<"`">>)), neotoma_util:p_anything()])]))), neotoma_util:p_string(<<"`">>)]), neotoma_util:p_string(<<"~">>)]))(I,D) end, fun(Node, Idx) -> 
   case Node of
       <<"~">> -> {code, <<"Node">>};
       _   -> {code, proplists:get_value('code', Node)}
   end
 end).



