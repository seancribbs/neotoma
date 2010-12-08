-module(peg_eqc).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

prop_charclass_matches() ->
    ?FORALL({String,Index},
            {non_empty(ascii_binary()), index()},
            begin
                Fun = neotoma_peg:p_charclass(charclass(String)),
                case Fun(String,Index) of
                    {M,R,_I} when is_binary(M), is_binary(R) -> true;
                    {fail, _} -> false
                end
            end).


prop_string() ->
    conjunction([{matches_self,prop_string_matches_self()},
                 {matches_head,prop_string_matches_head()},
                 {does_not_match,prop_string_does_not_match()}]).

prop_string_does_not_match() ->
    ?FORALL(String, non_empty(utf8_binary()),
            ?FORALL({Input, Index}, {non_matching_binary(String), index()},
                    begin
                        {fail, {expected, {string, String}, _}} = (neotoma_peg:p_string(String))(Input, Index),
                        true
                    end)).

prop_string_matches_head() ->
    ?FORALL({String,Tail,Index}, {utf8_binary(),utf8_binary(),index()},
            begin
                {String, Tail, _} = (neotoma_peg:p_string(String))(iolist_to_binary([String,Tail]), Index),
                true
            end).

prop_string_matches_self() ->
    ?FORALL({String,Index}, {utf8_binary(),index()},
            begin
                {String, <<>>, _} = (neotoma_peg:p_string(String))(String, Index),
                true
            end).

%% Generators
charclass(String) ->
    String2 = re:replace(String, "(\\[|\\])","\\&", [global, {return, binary}]),
    iolist_to_binary(["[",String2,"]"]).

index() ->
    {{line, nat()},{column, nat()}}.

ascii_char() ->
    choose(0,255).

ascii_string() ->
    list(ascii_char()).

ascii_binary() ->
    ?LET(L, ascii_string(), list_to_binary(L)).

utf8_char() ->
    choose(0,1024).

utf8_charlist() ->
    list(utf8_char()).

utf8_binary() ->
    ?SUCHTHAT(S, ?LET(L, utf8_charlist(), unicode:characters_to_binary(L,utf8,utf8)), is_binary(S)).

non_matching_binary(String) ->
    ?SUCHTHAT(Input,utf8_binary(),
              begin
                  Size = erlang:byte_size(String),
                  case Input of
                      <<>> -> false;
                      <<String:Size/binary, _Rest/binary>> -> false;
                      _ -> Size > 0
                  end
              end).
