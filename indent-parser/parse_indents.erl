#!/usr/bin/escript
%% -*- erlang -*-
%%! -smp enable

%% Example invocation:
%%     echo -e "a\n b\n c\n  d\n  d2\n e\nf\n g\n  h\n   i\n j\n k\nl\n" | ./parse_indents.erl :parse_indents:show_indented -

-module(parse_indents).
%% Need to export our output functions so we can invoke them.
-export([main/1, show_path/1, show_indented/1, build_nodes/3]).
%% Needs to be compiled so we can specify our output functions.
-mode(compile).


parse_text_line(Text) ->
    %% converts "    text" to (:content "text" :indent 4)
    %% strip spaces (by default) and tabs
    Content = string:strip(string:strip(Text, left), left, $\t),
    Indent = length(Text) - length(Content),
    %% Don't strip the line break before we've counted the indent
    {string:strip(Content, right, $\n), Indent}.


build_nodes(Parent, Group, []) -> Parent ! {Group, []};
build_nodes(Parent, Group, [{Content, Indent}|Rest]) ->
    %% split off our children from the rest of the Lines.
    %% the first line with an indent =< ours is a sibling, not a child
    IsChild = fun({_C, I}) -> I > Indent end,
    {ChildLines, SiblingLines} = lists:splitwith(IsChild, Rest),
    spawn(?MODULE, build_nodes, [self(), children, ChildLines]),
    spawn(?MODULE, build_nodes, [self(), siblings, SiblingLines]),
    Node = receive
        {children, Children} -> {Content, Children}
    end,
    receive
        {siblings, Siblings} ->
            io:fwrite( "[~s] ~p~n", [Content, self()]),
            Parent ! {Group, [Node|Siblings]}
    end.

%%--------------------------------------------------------------------------

show_indented(Nodes) -> show_indented(Nodes, 0).
show_indented(Nodes, Indent) ->
    %% print out the contents of a tree of nodes, indented appropriately
    Show = fun({Content, Children}) ->
        io:fwrite("~s~n", [string:chars($\s, Indent, Content)]),
        show_indented(Children, Indent + 4)
    end,
    lists:map(Show, Nodes).

get_node_path({Content, _Children}, "") -> Content;
get_node_path({Content, _Children}, ParentPath) ->
    string:join([ParentPath, Content], ":").

show_path(Nodes) -> show_path(Nodes, "").
show_path(Nodes, ParentPath) ->
    %% print out the leaves of a tree with their parent path (root::parent::child::leaf)
    Show = fun({Content, Children}) ->
        NodePath = get_node_path({Content, Children}, ParentPath),
        case Children of
            [] -> io:fwrite("~s~n", [NodePath]);
            _ -> show_path(Children, NodePath)
        end
    end,
    lists:map(Show, Nodes).

get_all_lines(Stream) -> get_all_lines(Stream, []).
get_all_lines(Stream, Lines) ->
    case io:get_line(Stream, "") of
        eof -> lists:reverse(Lines);
        Line -> get_all_lines(Stream, [Line|Lines])
    end.

process_stream(Stream, ShowFunc) ->
    %% build a tree from an input stream of indented text
    Text = get_all_lines(Stream),
    Lines = lists:map(fun(Line) -> parse_text_line(Line) end, Text),
    spawn(?MODULE, build_nodes, [self(), roots, Lines]),
    receive
        {roots, Nodes} -> ShowFunc(Nodes)
    end.

process_file("-", ShowFunc) ->
    process_stream(standard_io, ShowFunc);
process_file(Filename, ShowFunc) ->
    case file:open(Filename, [read]) of
        {ok, Handle} ->
            process_stream(Handle, ShowFunc),
            file:close(Handle);
        {error, Reason} -> io:fwrite("Error opening file ~s: ~s~n", [Filename, Reason])
    end.

%%--------------------------------------------------------------------------

main([]) ->
    io:format("usage: ~s.erl [-f <display_function>] <filenames...>~n", [?MODULE]),
    halt(1);
main(["-f", FuncName|Filenames]) ->
    Func = list_to_atom(FuncName),
    ShowFunc = fun(Nodes) -> apply(?MODULE, Func, [Nodes]) end,
    main(ShowFunc, Filenames);
main(Filenames) ->
    ShowFunc = fun(Nodes) -> show_path(Nodes) end,
    main(ShowFunc, Filenames).

main(ShowFunc, Filenames) ->
    ShowFile = fun(File) -> process_file(File, ShowFunc) end,
    lists:map(ShowFile, Filenames).

