#!/usr/local/bin/escript
-mode(compile).


main([]) ->
    print_loop(1).

print_loop(I) when I > 100 ->
    ok;
print_loop(I) ->
    io:format("~p~n", [fizzbuzz(I)]),
    print_loop(I + 1).

fizzbuzz(X) when ((X rem 5) == 0) and ((X rem 3) == 0) -> fizzbuzz;
fizzbuzz(X) when ((X rem 3) == 0) -> fizz;
fizzbuzz(X) when ((X rem 5) == 0) -> buzz;
fizzbuzz(X) -> X.

