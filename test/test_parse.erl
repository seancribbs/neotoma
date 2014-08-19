-module(test_parse).
-include_lib("eunit/include/eunit.hrl").

parser_test() ->
    % so we don't have to copy test.peg to .eunit
    Data = "rule <- .+;",
    file:write_file("test_parser.peg", io_lib:fwrite("~s\n", [Data])),
    neotoma:file("test_parser.peg"),
    compile:file("test_parser.erl", []),
    try 
        TestString =  [19990,30028,32,102,111,111],
        Result = test_parser:parse(TestString),
        ?assertEqual(6, length(Result)),
        StringResult = lists:flatten(io_lib:format("~ts", [Result])),
        ?assertEqual(TestString, StringResult)
    catch
        _:_  -> ?assert(false)
    end.

