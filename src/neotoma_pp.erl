%% @doc Turns grammar AST back into text.
-module(neotoma_pp).
-include("neotoma.hrl").
-export([print/1]).

print(#grammar{declarations=D, code=C}) ->
    [print(D1) || D1 <- D] ++ [ print(C) ];

print(#declaration{name=N, expr=E, code=C}) ->
    [ atom_to_binary(N, utf8), " <- ", print(E), print(C), ";\n\n"];

print(undefined) -> []; %% for empty code blocks
print(#code{identity=true}) -> " ~";
print(#code{code=B}=C) when is_binary(B) ->
    print(C#code{code=unicode:characters_to_list(B)});
print(#code{code=C}) ->
    case lists:member($\n, C) of
        true ->
            [" %{", C, "%}"];
        false ->
            [" `", C, "`"]
    end;

print(#sequence{exprs=[E0|Es]}) ->
    [ print(E0), [ [" ", print(E)] || E <- Es ] ];

print(#choice{alts=[A0|As]}) ->
    [ print(A0), [ [" / ", print(A) ] || A <- As] ];

print(#label{expr=E, label=L}) ->
    [ atom_to_binary(L, utf8), $:, maybe_paren(E) ];
print(#assert{expr=E}) ->
    [$&, maybe_paren(E)];
print(#deny{expr=E}) ->
    [$!, maybe_paren(E)];
print(#optional{expr=E}) ->
    [ maybe_paren(E), $? ];
print(#plus{expr=E}) ->
    [ maybe_paren(E), $+ ];
print(#star{expr=E}) ->
    [ maybe_paren(E), $* ];

print(#nonterminal{name=N}) ->
    atom_to_binary(N, utf8);

print(#anything{}) ->
    ".";

print(#epsilon{}) ->
    "<epsilon>";

print(#charclass{charclass=C}) ->
    [ $[, C, $] ];

print(#string{string=S}) ->
    Str = unicode:characters_to_list(S),
    {Quote, Escaped} = quote_and_escape_string(Str,
                                               lists:member($', Str),
                                               lists:member($", Str)),
    [Quote, Escaped, Quote];

print(#regexp{regexp=R}) ->
    [ $#, escape_chars(R, $#), $# ].


maybe_paren(Record) when ?IS_MODIFIER(Record) orelse
                         is_record(Record, sequence) orelse
                         is_record(Record, choice) ->
    [ $(, print(Record), $) ];
maybe_paren(Other) ->
    print(Other).


quote_and_escape_string(Str, false, true)  ->
    {$', escape_chars(Str, $')};
quote_and_escape_string(Str, _, _) ->
    {$", escape_chars(Str, $")}.

escape_chars(Bin, E) when is_binary(Bin) ->
    [ escape(C, E) || <<C/utf8>> <= Bin ];
escape_chars(Str, E) when is_list(Str) ->
    [ escape(C, E) || C <- Str ].

escape(C, C) ->
    [$\\, C];
escape(C, _E) ->
    C.


