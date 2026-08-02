-module(neotoma_ffi).
-export([format/1]).

format(Tree) ->
    Str = erl_prettypr:format(Tree, [{encoding, utf8}]),
    unicode:characters_to_binary(Str).
