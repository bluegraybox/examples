-module(game).
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

score(Rolls) ->
    frame(1, 0, Rolls).

%% frame/3 takes Frame #, Score accumulator, and list of remaining Rolls.
%%   It tail-recurses to the next frame with an updated Score and Rolls list.

%% Game complete.
frame(11, Score, _BonusRolls) -> Score;
%% Strike.
frame(Frame, Score, [10|Rest]) ->
    case Rest of
        [] -> Score + 10;
        [Bonus1] -> Score + 10 + Bonus1;
        [Bonus1, Bonus2|_] -> frame(Frame + 1, Score + 10 + Bonus1 + Bonus2, Rest)
    end;
%% Bad input.
frame(_Frame, _Score, [First,Second|_Rest]) when (First + Second > 10) -> err;
%% Spare.
frame(Frame, Score, [First,Second|Rest]) when (First + Second == 10) ->
    case Rest of
        [] -> Score + 10;
        [Bonus1|_] -> frame(Frame + 1, Score + 10 + Bonus1, Rest)
    end;
%% Normal.
frame(Frame, Score, [First,Second|Rest]) ->
    frame(Frame + 1, Score + First + Second, Rest);
%% Incomplete frames.
frame(_Frame, Score, [First]) -> Score + First;
frame(_Frame, Score, []) -> Score.

