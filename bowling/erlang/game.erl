-module(game).
-export([score/1]).

score(Rolls) ->
    frame(1, 0, Rolls).

%% frame/3 takes Frame #, Score accumulator, and list of remaining Rolls.
%%   It tail-recurses to the next frame with an updated Score and Rolls list.

%% Bad input - do this first so we check post-game bonus rolls.
frame(_Frame, _Score, [First|_Rest]) when (First < 0) or (First > 10) ->
    {invalid_roll, First};
frame(_Frame, _Score, [_First, Second|_Rest]) when (Second < 0) or (Second > 10) ->
    {invalid_roll, Second};
frame(_Frame, _Score, [First,Second|_Rest]) when (First /= 10) and (First + Second > 10) ->
    {invalid_total, First + Second};

% Game complete.
frame(11, Score, _BonusRolls) -> Score;

%% Strike.
frame(Frame, Score, [10|Rest]) ->
    frame(Frame + 1, Score + 10 + strike_bonus(Rest), Rest);

%% Spare.
frame(Frame, Score, [First,Second|Rest]) when (First + Second == 10) ->
    frame(Frame + 1, Score + 10 + spare_bonus(Rest), Rest);

%% Normal.
frame(Frame, Score, [First,Second|Rest]) ->
    frame(Frame + 1, Score + First + Second, Rest);

%% Incomplete frames.
frame(_Frame, Score, [First]) -> Score + First;
frame(_Frame, Score, []) -> Score.


%% spare & strike bonus calculations, with validation.
spare_bonus([]) -> 0;
spare_bonus([First|_Rest]) -> First.

strike_bonus([]) -> 0;
strike_bonus([Only]) -> Only;
strike_bonus([10,Second|_Rest]) -> 10 + Second;
strike_bonus([First,Second|_Rest]) -> First + Second.
