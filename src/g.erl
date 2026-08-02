-module(g).
-export([string/1]).

-spec string(binary()) -> {ok, term()} | {error, term()}.
string(Input) ->
    g(Input).

g(Input) ->
    case Input of
        <<"neotoma", Rest/binary>> -> {ok, {<<"neotoma">>, Rest}};
        _Other -> {error, no_match}
    end.
