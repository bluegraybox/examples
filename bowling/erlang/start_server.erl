#!/usr/bin/escript
%%! -smp enable -sname bowling_web

-mode(compile).  % for better performance

main([]) ->
    io:format("Usage: ~s <spooky install dir>~n", [escript:script_name()]);
main([SpookyDir]) ->
    %% Add spooky and its dependencies to the code path.
    true = code:add_path(SpookyDir ++ "/ebin"),
    Deps = filelib:wildcard(SpookyDir ++ "/deps/*/ebin"),
    ok = code:add_paths(Deps),
    %% Add this script's dir to the code path.
    true = code:add_path(filename:dirname(filename:absname(escript:script_name()))),

    %% Compile our modules, just to be safe.
    c:c(game),
    c:c(bowling_web),

    spooky:start_link(bowling_web),
    io:format("Started spooky~n"),

    case io:get_line("Return to exit...") of
        {error, Info} -> io:format("Error: ~p~n", [Info]);
        _Value -> store ! {self(), stop},
        %% wait for the store to shut down before exiting
        receive _Response -> ok end
    end,
    spooky:stop().

