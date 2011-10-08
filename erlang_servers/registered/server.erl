-module(server).
-export([start/2, rpc/2]).

%% Abstract server process; all business functionality is provided by a plugin module.


start(ServerId, PluginModule) ->
    State = PluginModule:init(),
    EventHandler = fun() -> event_handler(ServerId, PluginModule, State) end,
    register(ServerId, spawn(EventHandler)).


rpc(ServerId, Request) ->
    %% Synchronous call to server process.
    ServerId ! {self(), Request},
    receive
        {ServerId, Response} -> Response
    end.


%%% Internal functions

%% This is the server process; PluginModule:handle/2 does all the work.
event_handler(ServerId, PluginModule, State) ->
    receive
        {From, Request} ->
            {Response, NewState} = PluginModule:handle(Request, State),
            From ! {ServerId, Response},
            event_handler(ServerId, PluginModule, NewState)
    end.

