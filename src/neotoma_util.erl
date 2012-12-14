%% @author Slava Yurin <v.yurin@office.ngs.ru>
%% @doc Module for common parse action
-module(neotoma_util).

%% @type parse_fun() = function(Input::string(), Index::parse_index()) .
%% @type parse_index() = {{line, integer()},{column,integer()}} .
%% @type parse_result() = ({fail, Reason} | {Result::any(), Remainder::string(), NewIndex::parse_index()}) .

-export([p/4, p/5]).
-export([setup_memo/0, release_memo/0, memo_table_name/0]).

-export([
	p_eof/0
	, p_optional/1
	, p_not/1
	, p_assert/1
	, p_seq/1
	, p_and/1
	, p_choose/1
	, p_zero_or_more/1
	, p_one_or_more/1
	, p_label/2
	, p_string/1
	, p_anything/0
	, p_charclass/1
	, line/1
	, column/1
	]).


p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
	% See if the current reduction is memoized
	case get_memo(StartIndex, Name)
		of {ok, Memo} ->
			% If it is, return the stored result
			Memo
		; _ ->
			% If not, attempt to parse
			Result = case ParseFun(Inp, StartIndex)
				of {fail,_} = F ->
					% If it fails, memoize the failure
					F
				; {Match, InpRem, NewIndex} ->
					% If it passes, transform and memoize the result.
					Transformed = TransformFun(Match, StartIndex),
					{Transformed, InpRem, NewIndex}
			end,
			memoize(StartIndex, Name, Result),
			Result
	end.

get_memo(Index, Name) ->
	case ets:lookup(memo_table_name(), Index)
		of [] ->
			{error, not_found}
		; [{Index, Plist}] ->
			case proplists:lookup(Name, Plist)
				of {Name, Result}  ->
					{ok, Result}
				; _  ->
					{error, not_found}
			end
	end.

memoize(Index, Name, Result) ->
	Memo = case ets:lookup(memo_table_name(), Index)
		of [] ->
			[]
		; [{Index, M}] ->
			M
	end,
	ets:insert(memo_table_name(), {Index, [{Name, Result} | Memo]}).

setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

release_memo() ->
  ets:delete(memo_table_name()).

memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

p_eof() ->
	fun(<<>>, Index) ->
		{eof, [], Index}
	; (_, Index) ->
		{fail, {expected, eof, Index}}
	end.

p_optional(P) ->
	fun(Input, Index) ->
		case P(Input, Index)
			of {fail, _} ->
				{[], Input, Index}
			; {_, _, _} = Success ->
				Success
		end
	end.

p_not(P) ->
	fun(Input, Index) ->
		case P(Input,Index)
			of {fail, _} ->
				{[], Input, Index}
			; {Result, _, _} ->
				{fail, {expected, {no_match, Result}, Index}}
		end
	end.

p_assert(P) ->
	fun(Input,Index) ->
		case P(Input,Index)
			of {fail, _} = Failure ->
				Failure
			; _ ->
				{[], Input, Index}
		end
	end.

p_and(P) ->
	p_seq(P).

p_seq(P) ->
	fun(Input, Index) ->
		p_all(P, Input, Index, [])
	end.

p_all([], Inp, Index, Accum) ->
	{lists:reverse(Accum), Inp, Index};
p_all([P | Parsers], Inp, Index, Accum) ->
	case P(Inp, Index) of
		{fail, _} = Failure ->
			Failure
		; {Result, InpRem, NewIndex} ->
			p_all(Parsers, InpRem, NewIndex, [Result | Accum])
	end.

p_choose(Parsers) ->
	fun(Input, Index) ->
		p_attempt(Parsers, Input, Index, none)
	end.

p_attempt([], _Input, _Index, Failure) ->
	Failure;
p_attempt([P | Parsers], Input, Index, FirstFailure) ->
	case P(Input, Index)
		of {fail, _} = Failure when FirstFailure == none->
			p_attempt(Parsers, Input, Index, Failure)
		; {fail, _} ->
			p_attempt(Parsers, Input, Index, FirstFailure)
		; Result ->
			Result
	end.

p_zero_or_more(P) ->
	fun(Input, Index) ->
		p_scan(P, Input, Index, [])
	end.

p_one_or_more(P) ->
	fun(Input, Index) ->
		Result = p_scan(P, Input, Index, []),
		case Result
			of {[_ | _], _, _} ->
				Result
			; _ ->
				{fail, {expected, Failure, _}} = P(Input,Index),
				{fail, {expected, {at_least_one, Failure}, Index}}
		end
	end.

p_label(Tag, P) ->
	fun(Input, Index) ->
		case P(Input, Index)
			of {fail,_} = Failure ->
				Failure
			; {Result, InpRem, NewIndex} ->
				{{Tag, Result}, InpRem, NewIndex}
		end
	end.

p_scan(_, [], Index, Accum) ->
	{lists:reverse(Accum), [], Index};
p_scan(P, Inp, Index, Accum) ->
	case P(Inp, Index)
		of {fail, _} ->
			{lists:reverse(Accum), Inp, Index}
		; {Result, InpRem, NewIndex} ->
			p_scan(P, InpRem, NewIndex, [Result | Accum])
	end.

p_string(S) when is_list(S) ->
	p_string(list_to_binary(S));
p_string(S) ->
	Length = erlang:byte_size(S),
	fun(Input, Index) ->
		try
			<<S:Length/binary, Rest/binary>> = Input,
			{S, Rest, p_advance_index(S, Index)}
		catch
			error:{badmatch,_} ->
				{fail, {expected, {string, S}, Index}}
		end
	end.

p_anything() ->
	fun(<<>>, Index) ->
		{fail, {expected, any_character, Index}}
	; (Input, Index) when is_binary(Input) ->
		<<C/utf8, Rest/binary>> = Input,
		{<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
	end.

p_charclass(Class) ->
	{ok, RE} = re:compile(Class, [unicode, dotall]),
	fun(Inp, Index) ->
		case re:run(Inp, RE, [anchored])
			of {match, [{0, Length} | _]} ->
				{Head, Tail} = erlang:split_binary(Inp, Length),
				{Head, Tail, p_advance_index(Head, Index)}
			; _ ->
				{fail, {expected, {character_class, binary_to_list(Class)}, Index}}
		end
	end.

line({{line, L}, _}) ->
	L;
line(_) ->
	undefined.

column({_, {column, C}}) ->
	C;
column(_) ->
	undefined.

% string
p_advance_index(MatchedInput, Index) when
	is_list(MatchedInput); is_binary(MatchedInput)
->
	List = unicode:characters_to_list(MatchedInput),
	lists:foldl(fun p_advance_index/2, Index, List);
% one char
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) ->
	{{line, Line}, {column, Col}} = Index,
	case MatchedInput
		of $\n ->
			{{line, Line + 1}, {column, 1}}
		; _ ->
			{{line, Line}, {column, Col + 1}}
	end.
