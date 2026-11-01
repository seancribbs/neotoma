-module(neotoma_ffi).
-export([format/1, plus/0, parse_exprs/1, parse_forms/1]).

parse_forms(String) ->
    case unicode:characters_to_list(String, utf8) of
        Chars when is_list(Chars) ->
            parse_forms_loop(Chars, [], {1, 1});
        _ -> {error, nil}
    end.

parse_forms_loop([], Acc, _) ->
    {ok, lists:reverse(Acc)};

parse_forms_loop(Chars, Acc, Location) ->
    case erl_scan:tokens([], Chars, Location, []) of
        {done, {ok, Tokens, EndLocation}, LeftOverChars} ->
            case erl_parse:parse_form(Tokens) of
                {ok, AbsForm} ->
                    parse_forms_loop(LeftOverChars, [AbsForm|Acc], EndLocation);
                Error ->
                    io:format("Invalid form: ~n    Result: ~p~n    Input: ~p~n", [Error, Tokens]),
                    {error, nil}
            end;
        Error ->
            io:format("Invalid or incomplete tokens: ~n    Result: ~p~n    Remaining: ~p~n    Location: ~p~n", [Error, Chars, Location]),
            {error, nil}
    end.

parse_exprs(String) ->
    %% TODO: Pass in the location of the source snippet so that we can surface
    %% better errors
    case unicode:characters_to_list(String, utf8) of
        Chars when is_list(Chars) ->
            case erl_scan:string(Chars) of
                {ok, Tokens, Location} ->
                    Tokens2 = case lists:last(Tokens) of
                        {dot, _} -> Tokens;
                        _ -> lists:append([Tokens, [{dot, Location}]])
                    end,
                    case erl_parse:parse_exprs(Tokens2) of
                        {ok, Exprs} -> {ok, Exprs};
                        {error, ErrorInfo} ->
                            io:format("Invalid expression: ~p~nTokens: ~p~n", [ErrorInfo, Tokens]),
                            {error, nil}
                    end;
                {error, Info, _Location} ->
                    io:format("Invalid expression tokens: ~p~n", [Info]),
                    {error, nil}
            end;
        Error ->
            io:format("Bad unicode string: ~p~n", [Error]),
            {error, nil}
    end.

format(Tree) ->
    Str = erl_prettypr:format(Tree, [{encoding, utf8}]),
    unicode:characters_to_binary(Str).

plus() ->
    erl_syntax:operator('+').
