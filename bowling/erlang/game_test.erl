-module(game_test).
-import(game, [score/1]).
-export([test/0]).

test() ->
    test([
        [0,   [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0]],
        [20,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]],
        [6,   [1,1, 1,1, 1,1]], %% incomplete
        [150, [5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5]],
        [47,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10, 10 ,9]],
        [173, [7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 10]],
        [300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10]],
        [290, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10]],
        [300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10, 10, 10, 10]], %% extras
        [240, [10,  10,  10,  0,0, 10,  10,  10,  10,  10,  10,  10,  10]],
        [245, [10,  10,  10,  10,  10,  10,  10,  10,  10,  1,1]],
        [err, [10,  10,  5,6, 10,  10,  10,  10,  10,  10,  1,1]]]).

test(Tests) -> test(0, 0, Tests).
test(Pass, 0, []) -> io:fwrite("~nPassed! ~p tests~n", [Pass]);
test(Pass, Fail, []) -> io:fwrite("~nFailed! ~p fail, ~p pass~n", [Fail, Pass]);
test(Pass, Fail, [[Expected, Rolls]|Tests]) ->
    case score(Rolls) of
        Expected -> io:fwrite("."),
            test(Pass + 1, Fail, Tests);
        Scored -> io:fwrite("~nFail: expected=~p, scored=~p~n", [Expected, Scored]),
            test(Pass, Fail + 1, Tests)
    end.

