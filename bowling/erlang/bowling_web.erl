-module(bowling_web).
-behaviour(spooky).
-export([init/1, get/2, loop/1]).
-import(game).

%%% A REST web service for tracking bowling scores.

%% Uses the Spooky web framework; check it out from GitHub, build it, then:
%%     erlc -pa $SPOOKY_HOME/ebin/ bowling_web.erl game.erl && erl -pa $SPOOKY_HOME/ebin/ -pa $SPOOKY_HOME/deps/*/ebin
%% to compile this code and start up the Erlang shell. Once there, run:
%%     spooky:start_link(bowling_web).
%% to start the server on http://localhost:8000/.

init([])->
    %% register a process that holds our dict in memory
    case whereis(store) of
        undefined ->
            Pid = spawn(?MODULE, loop, [dict:new()]),
            register(store, Pid),
            io:format("store Pid=[~p]~n", [Pid]);
        Pid ->
            io:format("store already registered with Pid=[~p]~n", [Pid])
    end,
    [{port, 8000}].


get(Req, ["add", Player, RollText])->
    store ! {self(), Player, RollText},
    receive Score ->
        Req:ok(io_lib:format("~p", [Score]))
    end;

get(Req, ["score", Player])->
    store ! {self(), Player},
    receive Score ->
        Req:ok(io_lib:format("~p", [Score]))
    end;

get(Req, ["clear", Player])->
    store ! {self(), clear, Player},
    receive Score ->
        Req:ok(io_lib:format("~p", [Score]))
    end;

get(Req, [])->
    store ! {self(), restart},
    {ok, PageBytes} = file:read_file("form.html"),
    Page = binary_to_list(PageBytes),
    receive ok -> Req:ok(Page) end;

get(_Req, Path)->
    %% Assume these are static pages for the UI.
    Filename = filename:join(Path),
    case file:read_file(Filename) of
        {ok, PageBytes} -> {200, binary_to_list(PageBytes)};
        {error, Reason} -> {404, Reason}
    end.


loop(Dict) ->
    receive
        {Pid, restart} ->
            NewDict = dict:new(),
            Pid ! ok,
            loop(NewDict);
        {Pid, clear, Player} ->
            NewDict = dict:store(Player, [], Dict),
            Pid ! 0,
            loop(NewDict);
        {Pid, Player, RollText} ->
            Rolls = get_rolls(Player, Dict),
            {Roll, _} = string:to_integer(RollText),
            NewRolls = [Roll | Rolls],
            case game:score(lists:reverse(NewRolls)) of
                err -> NewDict = Dict,
                    Pid ! error;
                Score -> NewDict = dict:store(Player, NewRolls, Dict),
                    Pid ! Score
            end,
            loop(NewDict);
        {Pid, Player} ->
            Rolls = get_rolls(Player, Dict),
            Score = game:score(lists:reverse(Rolls)),
            Pid ! Score,
            loop(Dict)
    end.

get_rolls(Player, Dict) ->
    case dict:find(Player, Dict) of
        {ok, Rolls} -> Rolls;
        error -> []
    end.

