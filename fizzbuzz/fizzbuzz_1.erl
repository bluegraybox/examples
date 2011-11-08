#!/usr/local/bin/escript
-mode(compile).


main([]) -> fizzbuzz(1).

fizzbuzz(X) when X > 100 ->
    ok;
fizzbuzz(X) when ((X rem 5) == 0) and ((X rem 3) == 0) ->
    io:format("fizzbuzz~n"),
    fizzbuzz(X + 1);
fizzbuzz(X) when ((X rem 3) == 0) ->
    io:format("fizz~n"),
    fizzbuzz(X + 1);
fizzbuzz(X) when ((X rem 5) == 0) ->
    io:format("buzz~n"),
    fizzbuzz(X + 1);
fizzbuzz(X) ->
    io:format("~p~n", [X]),
    fizzbuzz(X + 1).


