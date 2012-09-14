-module(cheat_sheet_ipc).

-export([init/0, incr/1, decr/1, current/0]).  % not loop/0

%% Define a constant.
-define(SVC_NAME, counter_service).  % atom, not Variable or string


%% Start a new process to manage our counter.
init() ->
    %% MyFunc is a reference to a function
    MyFunc = fun () ->  % could all be on one line
        loop(0)
    end,
    %% Start a new process running MyFunc.
    Pid = spawn(MyFunc),  % pass the function reference; don't invoke it here.
    %% Register the Pid, so we can send it messages by name.
    register(?SVC_NAME, Pid).


%% Our message handling loop - not visible outside this module
loop(Current) ->
    NewCurrent = receive  % blocks until a new message comes in.
        {incr, Count} -> Current + Count;  % no response
        {decr, Count} -> Current - Count;
        {current, Pid} ->
            Pid ! {ok, Current},  % send the response
            Current;  % Return value so NewCurrent gets set
        Invalid ->  % catch everything else
            io:format("Invalid message: ~p~n", [Invalid]),
            Current
    end,
    loop(NewCurrent).  % tail-recurse with updated value


%% Wrapper functions for asynchronous sends.
incr(Count) -> ?SVC_NAME ! {incr, Count}.
decr(Count) -> ?SVC_NAME ! {decr, Count}.

%% Wrap async send/receive as synchronous function.
current() ->
    %% Send a message and wait for a response.
    ?SVC_NAME ! {current, self()},
    receive
        {ok, Current} ->
            io:format("Current value=~B~n", [Current])
    end.

