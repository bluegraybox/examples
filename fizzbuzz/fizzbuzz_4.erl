#!/usr/local/bin/escript
-mode(compile).


main([]) ->
    print_loop(fun fizzbuzz/1, 100, 1).

print_loop(_Func, Max, I) when I > Max ->
    ok;
print_loop(Func, Max, I) ->
    io:format("~p~n", [Func(I)]),
    print_loop(Func, Max, I + 1).

fizzbuzz(X) when ((X rem 5) == 0) and ((X rem 3) == 0) -> fizzbuzz;
fizzbuzz(X) when ((X rem 3) == 0) -> fizz;
fizzbuzz(X) when ((X rem 5) == 0) -> buzz;
fizzbuzz(X) -> X.

