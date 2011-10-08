## Example Erlang Servers
These are all examples of generic Erlang servers.  They're closely based on the examples that start on p. 286 in Joe Armstrong's _Programming Erlang_ book.

The idea is that there's a server module, which deals with the overhead of setting up and managing a server process, and there's a plugin module for the server that contains the business logic. The server is pure infrastructure; it has no application logic.  The plugin doesn't have to know anything about processes or IPC.

### registered
This is pretty close to Armstrong's original example. I've done a bit of refactoring and renaming to make it (hopefully) a little clearer, and added comments and unit tests.
kvstore.erl takes the place of name_server.erl.

### anonymous
This is a variant on the "registered" server. I'm not sure if it's an improvement, but it's an intersting experiment. Rather than registering the server process, it just uses the PID to talk to it. The strange bit is that rather than defining a normal RPC function, it creates it as a closure so that the client code never has to deal with the PID.
The kvstore runs with that idea, and returns all of its functions as closures to hide the RPC call.

It's worth noting that the init/0 and handle/2 implementations are the same in both examples.
