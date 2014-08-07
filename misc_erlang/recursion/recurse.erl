-module(recurse).

-export([countdown/0]).

countdown() ->
    countdown(100, 0).

countdown(0, Total) ->
    Total/0;
countdown(Val, Total) ->
    countdown(Val - 1, Total + 1).
