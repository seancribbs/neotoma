-module(peg_transform).
-author("Sean Cribbs <seancribbs@gmail.com>").
-export([parse_transform/2]).

% The parse transform for DRYing up parser files.
parse_transform(AST, _Options) ->
  transform_rules(AST).

transform_rules(AST) ->
  transform_rules(AST, []).

transform_rules([], Accum) ->
  lists:reverse(Accum);
transform_rules([{function,_Line,rule,1,Clauses}|Rest], Accum) ->
  Rules = lists:reverse(build_rules(Clauses)),
  transform_rules(Rest, Rules++Accum);
transform_rules([H|Rest], Accum) ->
  transform_rules(Rest, [H|Accum]).

build_rules(Clauses) ->
  lists:map(fun build_rule/1, Clauses).

build_rule({clause,Line,[{atom,_,Name}],_,Stmt}) ->
  Wrapped = wrap_reductions(Stmt, Name),
  {function,Line,Name,1,
    [{clause,Line,[{var,Line,'Input'}],[],Wrapped}]}.

wrap_reductions(Stmt,Name) ->
  Inner = hd(Stmt),
  Line = element(2, Inner), % [{call,Line,_,_}]
  Fun = wrap_fun(Inner, Line),
  [{call,Line,
    {remote,Line,{atom,Line,peg},{atom,Line,p}},
    [
      {var,Line,'Input'},
      {atom,Line,Name},
      Fun
    ]}].

wrap_fun(Stmts, Line) when is_tuple(Stmts) ->
   {'fun',Line,
    {clauses,
     [{clause,Line,
       [{var,Line,'I'}],
       [],
       [{call,Line,
         Stmts,
         [{var,Line,'I'}]}]}]}};
wrap_fun(Stmts, Line) ->
  io:format("peg rules must be single arity functions or statements and cannot be sequences of statements!~n"),
  throw({parse_error, not_tuple, Line, Stmts}).