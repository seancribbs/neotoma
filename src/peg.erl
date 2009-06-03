-module(peg).
-author("Sean Cribbs <seancribbs@gmail.com>").
% Giving credit to the original parser.erl library from which I lifted many
% of these functions, which in turn was based on the Haskell "parsec" library by
% Erik Meijer.  I've renamed the functions to be more Erlang-y.
-author("Jeffrey A. Meunier <jeffm@cse.uconn.edu>").

-export([p/3, p/4]).
-export([setup_memo/1, release_memo/0]).

-export([eof/0, optional/1,
         not_/1, assert/1, seq/1,
         and_/1, choose/1,
         zero_or_more/1, one_or_more/1,
         string/1, anything/0,
         charclass/1]).

%% Parsing wrapper for memoization
%% Let's be dumb initially and do no semantic analysis
p(Inp, Name, ParseFun) ->
  p(Inp, Name, ParseFun, fun(N) -> N end).
p(Inp, Name, ParseFun, TransformFun) ->
  % Record the starting index
  StartIndex = index(),
  % Grab the memo table from ets
  Memo = get_memo(StartIndex),
  % See if the current reduction is memoized
  case dict:find(Name, Memo) of
    % If it is, set the new index, and return the result
    {ok, {Result, NewIndex}} ->
      set_index(NewIndex),
      {Result, lists:nthtail(NewIndex - StartIndex, Inp)};
    % If not, attempt to parse
    _ ->
      case ParseFun(Inp) of
        % If it fails, reset the index, memoize the failure
        fail ->
          memoize(StartIndex, dict:store(Name, fail, Memo)),
          set_index(StartIndex),
          fail;
        % If it passes, advance the index, memoize the result.
        {Result, InpRem} ->
          NewIndex = StartIndex + (length(Inp) - length(InpRem)),
          Transformed = TransformFun(Result),
          memoize(StartIndex, dict:store(Name, {Transformed, NewIndex}, Memo)),
          set_index(NewIndex),
          {Result, InpRem}
      end
  end.


%% Memoizing results
setup_memo(Name) ->
  TID = ets:new(Name, [set]),
  put(ets_table, TID),
  set_index(0).

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

index() -> ets:lookup_element(get(ets_table), current_index, 2).
set_index(Value) -> ets:insert(get(ets_table), {current_index, Value}).


%% Parser combinators and matchers

eof() ->
  fun([]) -> {eof, []};
     (_) -> fail end.

optional(P) ->
  fun(Input) ->
      case P(Input) of
        fail -> {[], Input};
        {Result, InpRem} -> {Result, InpRem}
      end
  end.

% Negative lookahead
not_(P) ->
  fun(Input)->
      case P(Input) of
        fail ->
          {[], Input};
        _ -> fail
      end
  end.

% Positive lookahead
assert(P) ->
  fun(Input) ->
      case P(Input) of
        fail -> fail;
        _ -> {[], Input}
      end
  end.

and_(P) ->
  seq(P).
seq(P) ->
  fun(Input) ->
      all(P, Input, [])
  end.

all([], Inp, Accum ) -> {lists:reverse( Accum ), Inp};
all([P|Parsers], Inp, Accum) ->
  case P(Inp) of
    fail -> fail;
    {Result, InpRem} -> all(Parsers, InpRem, [Result|Accum])
  end.

choose(Parsers) ->
  fun(Input) ->
      attempt(Parsers, Input)
  end.

attempt([], _Input) -> fail;
attempt([P|Parsers], Input)->
  case P(Input) of
    fail -> attempt(Parsers, Input);
    Result -> Result
  end.

zero_or_more(P) ->
  fun(Input) ->
      scan(P, Input, [])
  end.

one_or_more(P) ->
  fun(Input)->
      Result = scan(P, Input, []),
      case Result of
        {[_|_], _} ->
          Result;
        _ -> fail
      end
  end.

scan(_, [], Accum) -> {lists:reverse( Accum ), []};
scan(P, Inp, Accum) ->
  case P(Inp) of
    fail -> {lists:reverse(Accum), Inp};
    {Result, InpRem} -> scan(P, InpRem, [Result | Accum])
  end.

string(S) ->
  fun(Input) ->
      case lists:prefix(S, Input) of
        true -> {S,  Input -- S};
        _ -> fail
      end
  end.

anything() ->
  fun([]) -> fail;
     ([H|T]) -> {H, T}
  end.

charclass(Class) ->
  fun(Inp) ->
     {ok, RE} = re:compile("^"++Class),
      case re:run(Inp, RE) of
        {match, _} ->
          {hd(Inp), tl(Inp)};
        _ -> fail
      end
  end.
