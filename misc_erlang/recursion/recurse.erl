-module(recurse).

-export([countdown/0]).

countdown() ->
    countdown(100, 0).

countdown(0, Total) ->
    Total/0;  %% generate error
countdown(Val, Total) ->
    %% recurse with decreased countdown and increased total
    countdown(Val - 1, Total + 1).
