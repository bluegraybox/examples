#!/usr/bin/escript
%% -*- erlang -*-
%%! -smp enable +P 1200300  % increase process limit

-module(ring).

%% Create a ring of N processes, then send a shutdown message.
%% Time how long these take.

-mode(compile).

-export([create/1, stop/2, start_process/2]).

-define(DBG(Msg, Opts), case os:getenv("verbose") of
    false -> ok;
    _ -> io:format(Msg, Opts)
end).

main([]) ->
    io:format("Usage: ./ring <number of processes>~n"),
    io:format("to get debugging info:~n"),
    io:format("    env verbose=y ./ring <N>~n"),
    io:format("(not recommended for more than 100 processes)~n");
main([Limit]) ->
    {LimitInt, _Rest} = string:to_integer(Limit),
    main(LimitInt);
main(Limit) ->
    Fun = fun() -> receive after infinity -> ok end end,
    {_,Bytes} = process_info(spawn(Fun), memory),
    Words = Bytes div erlang:system_info(wordsize),
    io:format("One process uses ~p bytes (~p words of ~p bytes each)~n", [Bytes, Words, erlang:system_info(wordsize)]),
    ProcMemInit = proplists:get_value(processes_used, erlang:memory()),

    io:format("Spawning ~p processes~n", [Limit]),
    {StartMicro, {ok, FirstPid, LastPid}} = timer:tc(ring, create, [Limit]),
    io:format("Started ~p processes in ~p seconds~n", [Limit, StartMicro/1000000]),

    ProcMemStarted = proplists:get_value(processes_used, erlang:memory()),
    io:format("Memory usage: ~p bytes~n", [ProcMemStarted - ProcMemInit]),

    {StopMicro, ok} = timer:tc(ring, stop, [FirstPid, LastPid]),
    io:format("Stopped ~p processes in ~p seconds~n", [Limit, StopMicro/1000000]).


create(N) ->
    FirstPid = spawn(ring, start_process, [N, self()]),
    % wait for a 'started' message from the last process
    receive
        {started, LastPid} ->
            {ok, FirstPid, LastPid}
    end.


start_process(0, ManagerPid) ->  % last process
    ?DBG("0!~n", []),
    %% Tell the manager we've started
    ManagerPid ! {started, self()},
    %% Wait for a stop message
    receive
        {stop, Pid} ->
            Pid ! {stop, self()}
    end;
start_process(Count, ManagerPid) ->
    ?DBG("~p.", [Count]),
    NextPid = spawn(ring, start_process, [Count-1, ManagerPid]),
    %% Wait for a stop message
    receive
        %% send it on to the next process and exit.
        {stop, Pid} -> NextPid ! {stop, Pid}
    end.


stop(FirstPid, LastPid) ->
    FirstPid ! {stop, self()},
    % wait for a stop message from the last process
    receive
        {stop, LastPid} -> ok
    end.
