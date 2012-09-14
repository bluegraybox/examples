-module(cheat_sheet).  % end with a period

%% Let these functions be called externally.
-export([countdown/1, countdown/0]).  % number of parameters - it matters!

%% atoms begin with a lower-case letter
%% Variables begin with an upper-case letter

%% Start defining a function with the most specific case first
countdown(0) ->
    io:format("Zero!~n");  % function clauses end with a semicolon

%% Another clause of the same function, with a guard expression
countdown(Bogus) when Bogus < 0 ->
    io:format("Bad value: ~B~n", [Bogus]),
    error;  % return value (io:format returns 'ok')

%% Last clause matches any single parameter
countdown(Start) ->
    %% case and if statements return values!
    Type = case Start rem 2 of
        1 -> "odd";  % case and if clauses end with semicolons
        0 -> "even"  % except the last one
    end,  % end with comma, like a normal line
    io:format("~B is ~s~n", [Start, Type]),
    countdown(Start - 1).  % When the function is all done, end with a period

%% This is a different function because it has a different number of parameters.
countdown() ->
    countdown(10).

