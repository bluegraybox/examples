-module(game).
-export([score/1]).

score(Rolls) -> frame(1, 0, Rolls).

%% frame/3 takes Frame #, Score accumulator, and list of remaining Rolls.
%%   It tail-recurses to the next frame with an updated Score and Rolls list.

% Game complete.
frame(11, Score, _BonusRolls) -> Score;

%% Strike.
frame(Frame, Score, [10|Rest]) ->
    case strike_bonus(Rest) of
        err -> err;
        Bonus -> frame(Frame + 1, Score + 10 + Bonus, Rest)
    end;

%% Bad input.
frame(_Frame, _Score, [First|_Rest]) when (First > 10) -> err;
frame(_Frame, _Score, [First,Second|_Rest]) when (First + Second > 10) -> err;

%% Spare.
frame(Frame, Score, [First,Second|Rest]) when (First + Second == 10) ->
    case spare_bonus(Rest) of
        err -> err;
        Bonus -> frame(Frame + 1, Score + 10 + Bonus, Rest)
    end;

%% Normal.
frame(Frame, Score, [First,Second|Rest]) ->
    frame(Frame + 1, Score + First + Second, Rest);

%% Incomplete frames.
frame(_Frame, Score, [First]) -> Score + First;
frame(_Frame, Score, []) -> Score.


%% spare & strike bonus calculations, with validation.
spare_bonus([]) -> 0;
spare_bonus([First|_Rest]) when (First > 10) -> err;
spare_bonus([First|_Rest]) -> First.

strike_bonus([]) -> 0;
strike_bonus([10]) -> 10;
strike_bonus([10,Second|_Rest]) when (Second > 10) -> err;
strike_bonus([10,Second|_Rest]) -> 10 + Second;
strike_bonus([First,Second|_Rest]) when (First + Second > 10) -> err;
strike_bonus([First,Second|_Rest]) -> First + Second.

