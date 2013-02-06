%% Calculate score for bowling game
%% Assumes valid and complete input
-module(game_happy).
-export([score/1]).

score(Rolls) -> frame(Rolls, 1, 0).

% Game complete.
frame(_BonusRolls, 11, Score) -> Score;

%% Strike.
frame([10|Rest], Frame, Score) ->
    frame(Rest, Frame + 1, Score + 10 + strike_bonus(Rest));

%% Spare.
frame([First,Second|Rest], Frame, Score) when (First + Second == 10) ->
    frame(Rest, Frame + 1, Score + 10 + spare_bonus(Rest));

%% Normal.
frame([First,Second|Rest], Frame, Score) ->
    frame(Rest, Frame + 1, Score + First + Second).

%% spare & strike bonus calculations.
spare_bonus([First|_Rest]) -> First.
strike_bonus([First,Second|_Rest]) -> First + Second.
