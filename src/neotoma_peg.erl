-module(neotoma_peg).
-author("Sean Cribbs <seancribbs@gmail.com>").

% Thanks to Jeffrey A. Meunier for the original parser.erl library from which I
% lifted many of these functions, which in turn was based on the Haskell
% "parsec" library by Erik Meijer.  I've renamed the functions to be more
% Erlang-y.

%% @type parse_fun() = function(Input::string(), Index::parse_index()) .
%% @type parse_index() = {{line, integer()},{column,integer()}} .
%% @type parse_result() = ({fail, Reason} | {Result::any(), Remainder::string(), NewIndex::parse_index()}) .

-export([p/4, p/5]).
-export([setup_memo/0, release_memo/0]).

-export([p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]).

-include("priv/peg_includes.hrl").
