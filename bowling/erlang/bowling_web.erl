-module(bowling_web).
-behaviour(spooky).
-export([init/1, get/2, loop/1]).
-import(game).

-define(DATA_FILE, "bowling_web.data").

%%% A REST web service for tracking bowling scores.

%% Uses the Spooky web framework; check it out from GitHub, build it, then run
%%     start_server.erl <Spooky dir>
%% to compile this code and start the Spooky server on http://localhost:8000/.

init([])->
    %% register a process that holds our dict in memory
    case whereis(store) of
        undefined ->
            Dict = case load(?DATA_FILE) of
                no_file -> dict:new();
                error -> dict:new();
                Data ->
                    io:format("Game loaded from ~s~n", [?DATA_FILE]),
                    Data
            end,
            Pid = spawn(?MODULE, loop, [Dict]),
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

get(Req, ["newgame"])->
    store ! {self(), newgame},
    receive ok ->
        Req:ok("")
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
        {Pid, stop} ->
            Pid !  save(?DATA_FILE, Dict);
        {Pid, all} ->
            Pid ! Dict,
            loop(Dict);
        {Pid, newgame} ->
            Pid ! ok,
            loop(dict:new());
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

load(Filename) ->
    case filelib:is_file(Filename) of
        true ->
            case file:open(Filename, [read]) of
                {ok, Handle} ->
                    Response = case io:read(Handle, "") of
                        {ok, Dict} -> Dict;
                        eof -> dict:new();
                        {error, Info} ->
                            io:format("Error: ~p~n", [Info]),
                            error
                    end,
                    file:close(Handle),
                    Response;
                {error, Reason} ->
                    io:format("Error opening file ~s: ~s~n", [Filename, Reason]),
                    error
            end;
        false -> io:format("No data file to load~n"), no_file
    end.

save(Filename, Dict) ->
    case file:open(Filename, [write]) of
        {ok, Handle} ->
            io:fwrite(Handle, "~w.", [Dict]),  % period to end the statement
            file:close(Handle),
            io:format("Game saved to ~s~n", [Filename]),
            ok;
        {error, Reason} ->
            io:format("Error opening file ~s: ~s~n", [Filename, Reason]),
            error
    end.

