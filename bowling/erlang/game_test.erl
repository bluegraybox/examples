#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -smp enable

-module(game_test).
%% Needs to be compiled so we can specify our output functions.
-mode(compile).

-import(game, [score/1]).
%%-export([test/0]).

main([]) ->
    test([
        {0,   [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0]},
        {20,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]},
        {{invalid_roll, 12}, [1,1, 12,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]},  % invalid roll
        {{invalid_roll, -1}, [1,1, 6,-1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]},  % invalid roll
        {{invalid_roll, -1}, [1,1, -1,6, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]},  % invalid roll
        {6,   [1,1, 1,1, 1,1]}, %% incomplete
        {18,  [1,1, 6,4, 3]}, %% incomplete w/ spare
        {150, [5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5]},
        {47,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10, 10 ,9]},
        {173, [7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 10]},
        {300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10]},
        {280, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  5]},  % incomplete
        {300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10, 10, 10, 10]}, %% extras
        {{invalid_total, 11}, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  9,2]},  % invalid extras
        {{invalid_roll, 11}, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  11]},  % invalid extras
        {{invalid_roll, 11}, [10,  10,  10,  10,  10,  10,  10,  10,  10,  9,1, 11]},  % invalid extras
        {240, [10,  10,  10,  0,0, 10,  10,  10,  10,  10,  10,  10,  10]},
        {245, [10,  10,  10,  10,  10,  10,  10,  10,  10,  1,1]},
        {{invalid_total, 11}, [10,  10,  5,6, 10,  10,  10,  10,  10,  10,  1,1]}]).

test(Tests) -> test(0, 0, Tests).
test(Pass, 0, []) -> io:fwrite("~nPassed! ~p tests~n", [Pass]);
test(Pass, Fail, []) -> io:fwrite("~nFailed! ~p fail, ~p pass~n", [Fail, Pass]);
test(Pass, Fail, [{Expected, Rolls}|Tests]) ->
    case score(Rolls) of
        Expected -> io:fwrite("."),
            test(Pass + 1, Fail, Tests);
        Scored -> io:fwrite("~nFailed test ~p: expected=~p, scored=~p~n", [Pass + Fail, Expected, Scored]),
            test(Pass, Fail + 1, Tests)
    end.

