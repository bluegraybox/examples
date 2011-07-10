#!/usr/bin/escript
%% -*- erlang -*-
%%! -smp enable

%% Example invocation:
%%     echo -e "a\n b\n c\n  d\n  d2\n e\nf\n g\n  h\n   i\n j\n k\nl\n" | ./parse_indents.erl :parse_indents:show_indented -

-module(parse_indents).
%% Need to export our output functions so we can invoke them.
-export([main/1, show_path/1, show_indented/1]).
%% Needs to be compiled so we can specify our output functions.
-mode(compile).
-record(line, {content, indent}).
-record(node, {content, children}).


parse_text_line(Text) ->
    %% converts "    text" to (:content "text" :indent 4)
    Content = string:strip(string:strip(Text, left), left, $\t),
    Indent = length(Text) - length(Content),
    #line{content=string:strip(Content, right, $\n), indent=Indent}.

split_list(_, []) -> [[],[]];
split_list(Criterion, DataList) ->
    %% break a list into two lists; the second begins with the first element that matches the criterion.
    [First|Rest] = DataList,
    case Criterion(First) of
        true ->
            [[], DataList];
        _ ->
            [ChildLines, SiblingLines] = split_list(Criterion, Rest),
            [[First]++ChildLines, SiblingLines]
    end.

build_nodes([]) -> [];
build_nodes(Lines) ->
    [First|Rest] = Lines,
    %% split off our children from the rest of the Lines.
    %% the first line with an indent =< ours is a sibling, not a child
    Criterion = fun(X) -> X#line.indent =< First#line.indent end,
    [ChildLines, SiblingLines] = split_list(Criterion, Rest),
    Children = build_nodes(ChildLines),
    Siblings = build_nodes(SiblingLines),
    Node = #node{content=First#line.content, children=Children},
    io:fwrite( "[~s]~n", [Node#node.content]),
    [Node]++Siblings.

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
    Nodes = build_nodes(Lines),
    ShowFunc(Nodes).

process_file(Filename, ShowFunc) ->
    case Filename of
        "-" -> process_stream(standard_io, ShowFunc);
        _ ->
            case file:open(Filename, [read]) of
                {ok, Handle} -> process_stream(Handle, ShowFunc);
                {error, Reason} -> io:fwrite("Error opening file ~s: ~s~n", [Filename, Reason])
            end
    end.

%%--------------------------------------------------------------------------

main([]) ->
    io:format("usage: parse_indents.erl [:function_name] <filenames...>\n"),
    halt(1);
main([[$\:|FuncName]|Filenames]) ->
    FuncList = lists:map(fun(List) -> list_to_atom(List) end, string:tokens(FuncName, ":")),
    ShowFunc = case FuncList of
        [Mod, Func] -> fun(Nodes) -> apply(Mod, Func, [Nodes]) end;
        [Func] -> fun(Nodes) -> apply(Func, [Nodes]) end
    end,
    main(ShowFunc, Filenames);
main(Filenames) ->
    ShowFunc = fun(Nodes) -> show_path(Nodes) end,
    main(ShowFunc, Filenames).

main(ShowFunc, Filenames) ->
    ShowFile = fun(File) -> process_file(File, ShowFunc) end,
    lists:map(ShowFile, Filenames).

