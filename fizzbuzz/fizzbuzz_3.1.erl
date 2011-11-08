#!/usr/local/bin/escript
-mode(compile).


main([]) ->
    Out = fizzbuzz(1, []),
    print(Out).

fizzbuzz(X, Out) when X > 100 ->
    lists:reverse(Out);
fizzbuzz(X, Out) when ((X rem 3) == 0) and ((X rem 5) == 0) ->
    fizzbuzz(X + 1, [fizzbuzz|Out]);
fizzbuzz(X, Out) when (X rem 3) == 0 ->
    fizzbuzz(X + 1, [fizz|Out]);
fizzbuzz(X, Out) when (X rem 5) == 0 ->
    fizzbuzz(X + 1, [buzz|Out]);
fizzbuzz(X, Out) ->
    fizzbuzz(X + 1, [X|Out]).

print([]) -> ok;
print([First|Rest]) ->
    io:format("~p~n", [First]),
    print(Rest).

