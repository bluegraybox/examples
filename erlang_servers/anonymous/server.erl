-module(server).
-export([start/1]).

%% Abstract server process; all business functionality is provided by a plugin module.

start(PluginModule) ->
    State = PluginModule:init(),
    EventHandler = fun() -> event_handler(PluginModule, State) end,
    Pid = spawn(EventHandler),
    %% Return our Remote Procedure Call (rpc/1) function as a closure
    fun(Request) ->
        %% Synchronous call to server process.
        Pid ! {self(), Request},
        receive
            {Pid, Response} -> Response
        end
    end.


%%% Internal functions

%% This is the server process; PluginModule:handle/2 does all the work.
event_handler(PluginModule, State) ->
    receive
        {From, Request} ->
            {Response, NewState} = PluginModule:handle(Request, State),
            From ! {self(), Response},
            event_handler(PluginModule, NewState)
    end.

