-module(game).
-export([score/1]).

score(Rolls) ->
    frame(1, 0, Rolls).

%% frame/3 takes Frame #, Score accumulator, and list of remaining Rolls.
%%   It tail-recurses to the next frame with an updated Score and Rolls list.

% Game complete.
frame(11, Score, _BonusRolls) -> Score;

%% Strike.
frame(Frame, Score, [10|Rest]) ->
    case Rest of
        [] -> Score + 10;
        [Bonus1] -> frame(Frame + 1, Score + 10 + Bonus1, Rest);
        [Bonus1, Bonus2|_] -> frame(Frame + 1, Score + 10 + Bonus1 + Bonus2, Rest)
    end;

%% Bad input.
frame(_Frame, _Score, [First|_Rest]) when (First > 10) -> err;
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

