%% #!/usr/bin/escript
%% -*- erlang -*-
%%! -smp enable +P 1200300  % increase process limit

-module(ring).

%% Create a ring of N processes, then send a shutdown message.
%% Time how long these take.

-mode(compile).

-export([start/1, start/2, start_quiet/2, stop/2, main/0, main/1, dump/0, ignore/0]).


main() -> main([]).

main([]) ->
    io:format("Usage: ./ring <number of processes>~n"),
    halt(1);
main([Limit]) ->
    case string:to_integer(Limit) of
        {error, Message} -> io:format("Bad parameter '~s': ~s~n", [Limit, Message]);
        {LimitInt, _Rest} -> go(LimitInt)
    end,
    halt().

go(Limit) ->
    Debugger = if Limit < 1000 -> dump; true -> ignore end,
    register(debugger, spawn(?MODULE, Debugger, [])),
    debugger ! "Test Message~n",
    Fun = fun() -> receive after infinity -> ok end end,
    {_,Bytes} = process_info(spawn(Fun), memory),
    Words = Bytes div erlang:system_info(wordsize),
    debugger ! {"One process uses ~p bytes (~p words of ~p bytes each)~n", [Bytes, Words, erlang:system_info(wordsize)]},
    ProcMemInit = proplists:get_value(processes_used, erlang:memory()),

    debugger ! {"Spawning ~p processes~n", [Limit]},
    {StartMicro, {ok, FirstPid, LastPid}} = timer:tc(?MODULE, start, [Limit]),
    debugger ! {"Started ~p processes in ~p seconds~n", [Limit, StartMicro/1000000]},

    ProcMemStarted = proplists:get_value(processes_used, erlang:memory()),
    debugger ! {"Memory usage: ~p bytes~n", [ProcMemStarted - ProcMemInit]},

    {StopMicro, ok} = timer:tc(?MODULE, stop, [FirstPid, LastPid]),
    debugger ! {"Stopped ~p processes in ~p seconds~n", [Limit, StopMicro/1000000]},

    debugger ! {done, self()},
    receive done -> ok end.


start(N) ->
    Start = if N < 1000 -> start; true -> start_quiet end,
    FirstPid = spawn(?MODULE, Start, [N, self()]),
    % wait for a 'started' message from the last process
    receive
        {started, LastPid} ->
            {ok, FirstPid, LastPid}
    end.

start(0, ManagerPid) ->  % last process
    debugger ! {count, 0},
    %% Tell the manager we've started
    ManagerPid ! {started, self()},
    wait_last();
start(Count, ManagerPid) ->
    debugger ! {count, Count},
    NextPid = spawn(?MODULE, start, [Count-1, ManagerPid]),
    wait(NextPid).


start_quiet(0, ManagerPid) ->  % last process
    %% Tell the manager we've started
    ManagerPid ! {started, self()},
    wait_last();
start_quiet(Count, ManagerPid) ->
    NextPid = spawn(?MODULE, start_quiet, [Count-1, ManagerPid]),
    wait(NextPid).

wait(NextPid) ->
    %% Wait for a stop message
    receive
        %% send it on to the next process and exit.
        {stop, FirstPid} -> NextPid ! {stop, FirstPid}
    end.

wait_last() ->
    %% Wait for a stop message
    receive
        {stop, FirstPid} ->
            FirstPid ! {stop, self()}
    end.

stop(FirstPid, LastPid) ->
    FirstPid ! {stop, self()},
    % wait for a stop message from the last process
    receive
        {stop, LastPid} -> ok
    end.

dump() ->
    receive
        {done, Pid} -> Pid ! done;
        {count, 0} -> io:format("0!~n");
        {count, Count} -> io:format("~p.", [Count]);
        {Fmt, Args} -> io:format(Fmt, Args);
        Msg -> io:format(Msg)
    end,
    dump().

ignore() ->
    receive
        {done, Pid} -> Pid ! done;
        {count, _Count} -> ok;
        {Fmt, Args} -> io:format(Fmt, Args);
        Msg -> io:format(Msg)
    end,
    ignore().
