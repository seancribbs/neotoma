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

print(#primary{label=L}=P) when L /= undefined ->
    [ atom_to_binary(L, utf8), $:, print(P#primary{label=undefined}) ];
print(#primary{modifier=assert}=P) ->
    [$&, print(P#primary{modifier=undefined})];
print(#primary{modifier=deny}=P) ->
    [$!, print(P#primary{modifier=undefined})];
print(#primary{modifier=optional}=P) ->
    [ print(P#primary{modifier=undefined}), $? ];
print(#primary{modifier=one_or_more}=P) ->
    [ print(P#primary{modifier=undefined}), $+ ];
print(#primary{modifier=zero_or_more}=P) ->
    [ print(P#primary{modifier=undefined}), $* ];
print(#primary{expr=Es}) when is_record(Es, sequence);
                              is_record(Es, choice) ->
    [ $(, print(Es), $) ];
print(#primary{expr=E}) ->
    print(E);

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

quote_and_escape_string(Str, false, true)  ->
    {$', escape_chars(Str, $')};
quote_and_escape_string(Str, _, _) ->
    {$", escape_chars(Str, $")}.

escape_chars(Str, E) ->
    [ escape(C, E) || C <- Str ].

escape(C, C) ->
    [$\\, C];
escape(C, _E) ->
    C.


