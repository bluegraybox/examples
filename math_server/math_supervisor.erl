-module(math_supervisor).
-behavior(supervisor).

-export([start/0, start_link/1, init/1]).
-export([start_in_shell_for_testing/0]).

start() ->
    spawn(fun() ->
            supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
        end).

start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{math_worker,
            {math_server, start_link, []},
            permanent,
            10000,
            worker,
            [math_server]}
          ]}}.

