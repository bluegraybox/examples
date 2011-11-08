#!/usr/local/bin/escript
-mode(compile).


main([]) ->
    Out = fizzbuzz(100, 1, []),
    print(Out);
main([MaxStr]) ->
    {Max, _} = string:to_integer(MaxStr),
    Out = fizzbuzz(Max, 1, []),
    print(Out).

fizzbuzz(Max, X, Out) when X > Max ->
    lists:reverse(Out);
fizzbuzz(Max, X, Out) when ((X rem 3) == 0) and ((X rem 5) == 0) ->
    fizzbuzz(Max, X + 1, [fizzbuzz|Out]);
fizzbuzz(Max, X, Out) when (X rem 3) == 0 ->
    fizzbuzz(Max, X + 1, [fizz|Out]);
fizzbuzz(Max, X, Out) when (X rem 5) == 0 ->
    fizzbuzz(Max, X + 1, [buzz|Out]);
fizzbuzz(Max, X, Out) ->
    fizzbuzz(Max, X + 1, [X|Out]).

print([]) -> ok;
print([First|Rest]) ->
    io:format("~p~n", [First]),
    print(Rest).

