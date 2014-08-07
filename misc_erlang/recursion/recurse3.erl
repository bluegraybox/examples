-module(recurse3).

-export([countdown/0]).


countdown() ->
    countdown(100, 0).

countdown(0, Total) ->
    Total/0;
countdown(Val, Total) ->
    bounce_one(Val - 1, Total + 1).

bounce_one(Val, Total) ->
    bounce_two(Val, Total + 1).

bounce_two(Val, Total) ->
    countdown(Val, Total + 1).
