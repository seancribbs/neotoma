-module(peg).
-author("Sean Cribbs <seancribbs@gmail.com>").

% Thanks to Jeffrey A. Meunier for the original parser.erl library from which I
% lifted many of these functions, which in turn was based on the Haskell
% "parsec" library by Erik Meijer.  I've renamed the functions to be more
% Erlang-y.

-export([p/4, p/5]).
-export([setup_memo/1, release_memo/0]).

-export([eof/0, optional/1,
         not_/1, assert/1, seq/1,
         and_/1, choose/1,
         zero_or_more/1, one_or_more/1,
         label/2,
         string/1, anything/0,
         charclass/1]).
-define(MEMOIZE, true).
%% Parsing wrapper for memoization
p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N) -> N end).
-ifdef(MEMOIZE).
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  % Record the starting index
  % StartIndex = index(),
  % Grab the memo table from ets
  Memo = get_memo(StartIndex),
  % See if the current reduction is memoized
  case dict:find(Name, Memo) of
    % If it is, set the new index, and return the result
    {ok, Result} -> Result;
    % If not, attempt to parse
    _ ->
      case ParseFun(Inp, StartIndex) of
        % If it fails, reset the index, memoize the failure
        {fail,_} = Failure ->
          memoize(StartIndex, dict:store(Name, Failure, Memo)),
          Failure;
        % If it passes, advance the index, memoize the result.
        {Result, InpRem, NewIndex} ->
          %io:format("Result <~p>: ~p~n", [Name, [Result]]),
          Transformed = TransformFun(Result),
          memoize(StartIndex, dict:store(Name, {Transformed, InpRem, NewIndex}, Memo)),
          %set_index(NewIndex),
          {Transformed, InpRem, NewIndex}
      end
  end.
-else.
p(Inp,StartIndex,_Name,ParseFun,TransformFun) ->
  case ParseFun(Inp,StartIndex) of
    {fail,I} ->
       {fail,I};
    {Result, NewInp, NewIndex} ->
      {TransformFun(Result), NewInp, NewIndex}
  end.
-endif.

%% Memoizing results
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


%% Parser combinators and matchers

eof() ->
  fun([], Index) -> {eof, [], Index};
     (_, Index) -> {fail, Index} end.

optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

% Negative lookahead
not_(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        _ -> {fail, Index}
      end
  end.

% Positive lookahead
assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} -> {fail, Index};
        _ -> {[], Input, Index}
      end
  end.

and_(P) ->
  seq(P).
seq(P) ->
  fun(Input, Index) ->
      all(P, Input, Index, [])
  end.

all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, I} -> {fail, I};
    {Result, InpRem, NewIndex} -> all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.

choose(Parsers) ->
  fun(Input, Index) ->
      attempt(Parsers, Input, Index)
  end.

attempt([], _Input, Index) -> {fail, Index};
attempt([P|Parsers], Input, Index)->
  case P(Input, Index) of
    {fail, _} -> attempt(Parsers, Input, Index);
    Result -> Result
  end.

zero_or_more(P) ->
  fun(Input, Index) ->
      scan(P, Input, Index, [])
  end.

one_or_more(P) ->
  fun(Input, Index)->
      Result = scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ -> {fail, Index}
      end
  end.

label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} ->
           {fail, Index};
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.

scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> scan(P, InpRem, NewIndex, [Result | Accum])
  end.

string(S) ->
  fun(Input, Index) ->
      case lists:prefix(S, Input) of
        true -> {S, lists:sublist(Input, length(S)+1, length(Input)), Index + length(S)};
        _ -> {fail, Index}
      end
  end.

anything() ->
  fun([], Index) -> {fail, Index};
     ([H|T], Index) -> {H, T, Index+1}
  end.

charclass(Class) ->
  fun(Inp, Index) ->
     {ok, RE} = re:compile("^"++Class),
      case re:run(Inp, RE) of
        {match, _} ->
          {hd(Inp), tl(Inp), Index+1};
        _ -> {fail,Index}
      end
  end.
