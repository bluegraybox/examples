-module(kvstore).

%% Plugin module for server.

-import(server).
-export([init/0, handle/2]).  % Implement the Server callback interface.
-export([start_server/0, getKV/1, setKV/2]).  % Export our own API functions.
-export([test/0]).  % Export our unit tests.

-define(SERVER_ID, kv_server).


%% Client API functions
start_server()    -> server:start(?SERVER_ID, ?MODULE).
setKV(Key, Value) -> server:rpc(?SERVER_ID, {setKV, Key, Value}).
getKV(Key)        -> server:rpc(?SERVER_ID, {getKV, Key}).


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
    assertEq(true, start_server()),
    assertEq([], getKV(test)),
    assertEq(ok, setKV(test, "Test Message")),
    assertEq("Test Message", getKV(test)),
    assertEq([], getKV(bogus)),
    assertEq([], getKV(test1)),
    assertEq(ok, setKV(test, "Test Message Two")),
    assertEq("Test Message Two", getKV(test)).

assertEq(Expected, Expected) -> io:format("pass~n");
assertEq(Expected, Actual)   -> io:format("fail: expected=~p, actual=~p~n", [Expected, Actual]).

