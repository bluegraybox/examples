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
-record(line, {content, indent}).
-record(node, {content, children}).


parse_text_line(Text) ->
    %% converts "    text" to (:content "text" :indent 4)
    Content = string:strip(string:strip(Text, left), left, $\t),
    Indent = length(Text) - length(Content),
    #line{content=string:strip(Content, right, $\n), indent=Indent}.


build_nodes(Parent, Group, []) -> Parent ! {Group, []};
build_nodes(Parent, Group, [First|Rest]) ->
    %% split off our children from the rest of the Lines.
    %% the first line with an indent =< ours is a sibling, not a child
    IsChild = fun(L) -> L#line.indent > First#line.indent end,
    {ChildLines, SiblingLines} = lists:splitwith(IsChild, Rest),
    spawn(?MODULE, build_nodes, [self(), children, ChildLines]),
    spawn(?MODULE, build_nodes, [self(), siblings, SiblingLines]),
    receive
        {children, Children} ->
            Node = #node{content=First#line.content, children=Children}
    end,
    receive
        {siblings, Siblings} ->
            io:fwrite( "[~s] ~p~n", [Node#node.content, self()]),
            Parent ! {Group, [Node|Siblings]}
    end.

%%--------------------------------------------------------------------------

show_indented(Nodes) -> show_indented(Nodes, 0).
show_indented(Nodes, Indent) ->
    %% print out the contents of a tree of nodes, indented appropriately
    Show = fun(Node) ->
        io:fwrite("~s~n", [string:chars($\s, Indent, Node#node.content)]),
        show_indented(Node#node.children, Indent + 4)
    end,
    lists:map(Show, Nodes).

get_node_path(Node, "") -> Node#node.content;
get_node_path(Node, ParentPath) ->
    Content = Node#node.content,
    string:join([ParentPath, Content], ":").

show_path(Nodes) -> show_path(Nodes, "").
show_path(Nodes, ParentPath) ->
    %% print out the leaves of a tree with their parent path (root::parent::child::leaf)
    Show = fun(Node) ->
        NodePath = get_node_path(Node, ParentPath),
        case Node#node.children of
            [] -> io:fwrite("~s~n", [NodePath]);
            Children -> show_path(Children, NodePath)
        end
    end,
    lists:map(Show, Nodes).

get_all_lines(Stream) -> get_all_lines(Stream, []).
get_all_lines(Stream, Lines) ->
    case io:get_line(Stream, "") of
        eof -> file:close(Stream), Lines;
        Line -> get_all_lines(Stream, Lines ++ [Line])
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
        {ok, Handle} -> process_stream(Handle, ShowFunc);
        {error, Reason} -> io:fwrite("Error opening file ~s: ~s~n", [Filename, Reason])
    end.

%%--------------------------------------------------------------------------

main([]) ->
    io:format("usage: ~s.erl [[:module]:display_function] <filenames...>~n", [?MODULE]),
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

