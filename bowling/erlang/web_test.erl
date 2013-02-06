#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -smp enable

-mode(compile).  % for better performance


main(_) ->
    inets:start(),
    get_url("http://localhost:8000/newgame"),
    Tests = [
        {"Joe", 4, "4"},
        {"Joe", 9, "{invalid_total,13}"}, % bad input
        {"John", 4, "4"},
        {"John", 5, "9"},
        {"John", 6, "15"},
        {"John", 4, "19"},
        {"John", 3, "25"}, % spare
        {"John", 2, "27"},
        {"John", 10, "37"},
        {"John", 2, "41"}, % strike
        {"John", 6, "53"}, % strike
        %% interleaving players
        {"Fred", 3, "3"},
        {"Dave", 2, "2"},
        {"Fred", 4, "7"},
        {"Dave", 6, "8"},
        {"Fred", 1, "8"},
        {"Dave", 7, "15"},
        {"Fred", 2, "10"},
        {"Dave", 3, "18"}],
    test(Tests).

test(Tests) -> test(Tests, 0, 0).

test([], Pass, 0)    -> io:format("Passed! (~p tests)~n", [Pass]);
test([], Pass, Fail) -> io:format("Failed! Passed ~p, Failed ~p~n", [Pass, Fail]);
test([{Player, Roll, Expected} | Tests], Pass, Fail) ->
    Url = io_lib:format("http://localhost:8000/add/~s/~p", [Player, Roll]),
    % io:format("Testing URL: ~s~n", [Url]),
    case get_url(Url) of
        Expected -> io:format(". "),
            test(Tests, Pass + 1, Fail);
        Actual -> io:format("Fail: expected=~p, actual=~p~n", [Expected, Actual]),
            test(Tests, Pass, Fail + 1)
    end.

get_url(Url) ->
    case httpc:request(Url) of
        {ok, {_Status, _Header, Content}} -> Content;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

