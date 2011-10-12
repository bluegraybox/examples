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

get(Req, ["all"])->
    store ! {self(), all},
    receive Dict ->
        Json = to_json(dict:to_list(Dict), []),
        % io:format("Json: ~s~n", [Json]),
        Req:ok(Json)
    end;

get(Req, [])->
    {ok, PageBytes} = file:read_file("form.html"),
    Page = binary_to_list(PageBytes),
    Req:ok(Page);

get(_Req, Path)->
    %% Assume these are static pages for the UI.
    Filename = filename:join(Path),
    case file:read_file(Filename) of
        {ok, PageBytes} -> {200, binary_to_list(PageBytes)};
        {error, Reason} -> {404, Reason}
    end.

to_json([{Name, Rolls} | Rest], Scores) ->
    Score = game:score(Rolls),
    Json = io_lib:format("{\"name\": \"~s\", \"rolls\": ~w, \"score\": ~w}", [Name, Rolls, Score]),
    to_json(Rest, [Json | Scores]);

to_json([], Scores) -> "[" ++ string:join(lists:reverse(Scores), ",") ++ "]".


loop(Dict) ->
    receive
        {Pid, all} ->
            Pid ! Dict,
            loop(Dict);
        {Pid, clear, Player} ->
            NewDict = dict:erase(Player, Dict),
            Pid ! 0,
            loop(NewDict);
        {Pid, Player, RollText} ->
            {Roll, _} = string:to_integer(RollText),
            NewDict = dict:append(Player, Roll, Dict),
            Rolls = dict:fetch(Player, NewDict),
            case game:score(Rolls) of
                err ->
                    Pid ! error,
                    loop(Dict);
                Score ->
                    Pid ! Score,
                    loop(NewDict)
            end;
        {Pid, Player} ->
            Score = case dict:find(Player, Dict) of
                {ok, Rolls} -> game:score(Rolls);
                error -> 0
            end,
            Pid ! Score,
            loop(Dict)
    end.

