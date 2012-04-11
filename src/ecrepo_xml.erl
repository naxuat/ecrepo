-module(ecrepo_xml).

-export([
    document/1,
    elem/1,
    format_to_bin/2
]).

-define(HEADER, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n").

-define(N(Arg), ecrepo_xml:format_to_bin("~b", [Arg])).

document(RootElement) ->
    Output = elem(RootElement, 0),
    <<?HEADER, Output/binary>>.

elem(Element) ->
    elem(Element, 0).

format_to_bin(Format, Args) ->
    list_to_binary(io_lib:format(Format, Args)).

elem(Tag, Indent) when is_atom(Tag) ->
    elem2bin({Tag, [], []}, Indent);
elem({Tag, Content}, Indent) ->
    elem2bin({Tag, [], Content}, Indent);
elem(Item, Indent) ->
    elem2bin(Item, Indent).

elem2bin({Tag, Attrs, Content}, Indent) ->
    TagName = tag2name(Tag),
    Preamble = <<"<", TagName/binary, (attrs2bin(Attrs))/binary>>,
    case Content of
        Blah when Blah == []; Blah == <<>> ->
            <<Preamble/binary, "/>\n">>;

        _ ->
            <<Preamble/binary, ">", (content2bin(Content, Indent + 2))/binary, "</", TagName/binary, ">\n">>
    end.

tag2name(Tag) when is_atom(Tag) ->
    atom_to_binary(Tag, latin1);
tag2name({NS, Tag}) when is_atom(NS), is_atom(Tag) ->
    <<(atom_to_binary(NS, latin1))/binary, ":", (atom_to_binary(Tag, latin1))/binary>>.

attrs2bin(Attrs) ->
    attrs2bin(Attrs, <<>>).

attrs2bin([{Name, Value} | Rest], Output) ->
    ActualValue = if
        is_integer(Value) ->
            ?N(Value);

        is_binary(Value) ->
            ecrepo_lib:quote(Value);

        true ->
            throw({badarg, Value})
    end,
    attrs2bin(Rest, <<Output/binary, " ", (tag2name(Name))/binary, "=\"", ActualValue/binary, "\"">>);
attrs2bin([], Output) ->
    Output.

content2bin(Content, _) when is_binary(Content) ->
    ecrepo_lib:quote(Content);
content2bin(Content, _) when is_number(Content) ->
    ?N(Content);
content2bin(Content, Indent) ->
    content2bin(Content, Indent, format_to_bin("~*s", [Indent, " "]), <<"\n">>).

content2bin([Binary | Rest], Indent, Prefix, Output) when is_binary(Binary) ->
    content2bin(Rest, Indent, Prefix,
                <<Output/binary, Binary/binary>>);
content2bin([Elem | Rest], Indent, Prefix, Output) ->
    content2bin(Rest, Indent, Prefix,
                <<Output/binary, Prefix/binary, (elem(Elem, Indent))/binary>>);
content2bin([], _Indent, <<_, _, DeIndent/binary>>, Output) ->
    <<Output/binary, DeIndent/binary>>.