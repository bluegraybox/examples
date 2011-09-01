-module(math_server).
-behavior(gen_server).
-vsn("0.1.0").

%% main funcs
-export([multiply/2, divide/2]).
%% startup func
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

version() ->
    "0.1".

multiply_impl(X, Y) ->
    io:format("multiply_impl() v.~s invoked~n", [version()]),
    X * Y.

divide_impl(X, Y) ->
    io:format("divide_impl() v.~s invoked~n", [version()]),
    X * Y.  % FIXME: Deliberate copy-paste error!

%%----------------------------------------------------------------------
%% Client and server functions:
%%
%% The user invokes multiply() and divide(), but behind the scenes, these are just client functions that send a request to the server.
%% The request goes: multiply(X,Y) -> handle_call({multiply,X,Y}) -> multiply_impl(X,Y)
multiply(X, Y) ->
    gen_server:call(?MODULE, {multiply, X, Y}).

divide(X, Y) ->
    gen_server:call(?MODULE, {divide, X, Y}).

%% These receive the request, call the implementation functions, and return the results.
handle_call({multiply, X, Y}, _From, N) -> {reply, multiply_impl(X, Y), N+1};

handle_call({divide, X, Y}, _From, N) -> {reply, divide_impl(X, Y), N+1}.

%%----------------------------------------------------------------------
%% The rest of these are boilerplate gen_server functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p v.~s initialized~n", [?MODULE, version()]),
    {ok, 0}.

handle_cast(_Msg, N) -> {noreply, N}.

handle_info(_Info, N) -> {noreply, N}.

terminate(_Reason, _N) ->
    io:format("~p v.~s terminated~n", [?MODULE, version()]),
    ok.

code_change(OldVsn, N, _Extra) ->
    io:format("~p v.~s updated from v. ~s~n", [?MODULE, version(), OldVsn]),
    {ok, N}.

