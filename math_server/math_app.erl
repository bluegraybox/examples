%% cribbed from "Programming Erlang" p. 352
-module(math_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) -> math_supervisor:start_link(StartArgs).

stop(_State) -> ok.

