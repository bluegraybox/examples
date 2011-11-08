#!/usr/local/bin/escript
-mode(compile).


main([]) ->
    fizzbuzz(100, 1);
main([MaxStr]) ->
    {Max, _} = string:to_integer(MaxStr),
    fizzbuzz(Max, 1).

fizzbuzz(Max, X) when X > Max ->
    ok;
fizzbuzz(Max, X) when ((X rem 5) == 0) and ((X rem 3) == 0) ->
    io:format("fizzbuzz~n"),
    fizzbuzz(Max, X + 1);
fizzbuzz(Max, X) when ((X rem 3) == 0) ->
    io:format("fizz~n"),
    fizzbuzz(Max, X + 1);
fizzbuzz(Max, X) when ((X rem 5) == 0) ->
    io:format("buzz~n"),
    fizzbuzz(Max, X + 1);
fizzbuzz(Max, X) ->
    io:format("~p~n", [X]),
    fizzbuzz(Max, X + 1).


