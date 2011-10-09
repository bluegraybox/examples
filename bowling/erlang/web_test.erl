#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -smp enable

-module(web_test).
%% Needs to be compiled so we can specify our output functions.
-mode(compile).

%% Need to export our output functions so we can invoke them.
-export([main/1]).

main(_) ->
    inets:start(),
    get_url("http://localhost:8000/clear/Joe"),
    Tests = [
        {"Joe", 4, "4"},
        {"Joe", 9, "error"},
        {"Joe", 5, "9"},
        {"Joe", 6, "15"},
        {"Joe", 4, "19"},
        {"Joe", 3, "25"},
        {"Joe", 2, "27"},
        {"Joe", 10, "37"},
        {"Joe", 2, "41"},
        {"Joe", 6, "53"}],
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

