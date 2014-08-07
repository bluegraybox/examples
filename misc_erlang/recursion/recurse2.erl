-module(recurse2).

-export([countdown/0]).

%% non-tail recursion
countdown() ->
    countdown(10).

countdown(0) ->
    1/0;
countdown(Val) ->
    1 + countdown(Val - 1).
