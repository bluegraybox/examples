-module(game).
-export([test/0]).

test() ->
    test(0,   [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0]),
    test(20,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]),
    test(150, [5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5]),
    test(47,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10, 10 ,9]),
    test(173, [7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 10]),
    test(300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10]),
    test(300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10, 10, 10, 10]),
    test(245, [10,  10,  10,  10,  10,  10,  10,  10,  10,  1,1]),
    test(err, [10,  10,  5,6, 10,  10,  10,  10,  10,  10,  1,1]).

test(Expected, Rolls) ->
    case score(Rolls) of
        Expected -> io:fwrite("Pass~n");
        Scored -> io:fwrite("Fail: expected=~p, scored=~p~n", [Expected, Scored])
    end.

score(Rolls) ->
    frame(1, 0, Rolls).

%% frame/3 takes Frame #, Score accumulator, and list of remaining Rolls.
%%   It tail-recurses to the next frame with an updated Score and Rolls list.

%% Game complete.
frame(11, Score, _BonusRolls) -> Score;
%% Strike.
frame(Frame, Score, [10|Rest]) ->
    [Bonus1, Bonus2|_] = Rest,
    frame(Frame + 1, Score + 10 + Bonus1 + Bonus2, Rest);
%% Bad input.
frame(_Frame, _Score, [First,Second|_Rest]) when (First + Second > 10) -> err;
%% Spare.
frame(Frame, Score, [First,Second|Rest]) when (First + Second == 10) ->
    [Bonus1|_] = Rest,
    frame(Frame + 1, Score + 10 + Bonus1, Rest);
%% Normal.
frame(Frame, Score, [First,Second|Rest]) ->
    frame(Frame + 1, Score + First + Second, Rest).

