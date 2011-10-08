-module(kvstore).

%% Plugin module for server.

-import(server).
-export([init/0, handle/2]).  % Implement the Server callback interface.
-export([start_server/0]).  % Export our own API functions.
-export([test/0]).  % Export our unit tests.


%% Client API functions
start_server() ->
    RPC = server:start(?MODULE),
    %% Create closures for each of our functions
    Set = fun(Key, Value) -> RPC({setKV, Key, Value}) end,
    Get = fun(Key) -> RPC({getKV, Key}) end,
    {{setKV, Set}, {getKV, Get}}.


%% Server callback handlers
init() -> dict:new().

%% {Response, NewState} = handle(Request, State),
handle({setKV, Key, Value}, Dict) -> {ok, dict:store(Key, Value, Dict)};
handle({getKV, Key}, Dict)        ->
    case dict:find(Key, Dict) of
        {ok, Value} -> {Value, Dict};
        error       -> {[], Dict}
    end.


%%% Unit Tests

test() ->
    {{setKV, Set}, {getKV, Get}} = start_server(),
    assertEq([], Get(test)),
    assertEq(ok, Set(test, "Test Message")),
    assertEq("Test Message", Get(test)),
    assertEq([], Get(bogus)),
    assertEq([], Get(test1)),
    assertEq(ok, Set(test, "Test Message Two")),
    assertEq("Test Message Two", Get(test)).

assertEq(Expected, Expected) -> io:format("pass~n");
assertEq(Expected, Actual)   -> io:format("fail: expected=~p, actual=~p~n", [Expected, Actual]).

