%% Calculate score for bowling game
%% Assumes valid and complete input
-module(game_happy).
-export([score/1]).

score(Rolls) -> frame(1, 0, Rolls).

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
    frame(Frame + 1, Score + First + Second, Rest).

%% spare & strike bonus calculations.
spare_bonus([First|_Rest]) -> First.
strike_bonus([First,Second|_Rest]) -> First + Second.
