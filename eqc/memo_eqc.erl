%%% @author Sean Cribbs <sean@seanbasho>
%%% @copyright (C) 2010, Sean Cribbs
%%% @doc
%%%
%%% @end
%%% Created : 27 Apr 2010 by Sean Cribbs <sean@seanbasho>

-module(memo_eqc).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

parse_fun() ->
    oneof([
           fun(Inp,Idx) ->
                   {fail, {expected, pass, Idx}}
           end,
           fun(Inp,Idx) ->
                  {[], Inp, Idx}
           end
           ]).
