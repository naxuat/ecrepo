-module(ecrepo).
-vsn(?VERSION).

-include_lib("kernel/include/file.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ecrepo_common.hrl").

-export([
    read/1,
    str2evr/1
]).

-define(EVR_RE, <<"^((?P<E>[^:]+):)?(?P<V>[^-]*)(-(?P<R>[^-]+))?$">>).

read(FileName) ->
    {ok, Data} = ecrepo_lib:header(FileName),
    {ok, HeaderStart, HeaderEnd} = get_header_range(FileName),
    {ok, Size, MTime} = get_info(FileName),
    {sha256, Sum} = lists:keyfind(sha256, 1, esums_file:calc(FileName)),
    {ok, normalize(Data, [{header_start, HeaderStart},
                          {header_end, HeaderEnd},
                          {filesize, Size},
                          {filetime, MTime},
                          {sha256, esums:format(sha256, Sum)},
                          {location, list_to_binary(FileName)}])}.

str2evr(String) when is_binary(String) ->
    case re:run(String, ?EVR_RE, [{capture, ['E', 'V', 'R'], binary}]) of
        {match, [<<>>, V, R]} ->
            {<<"0">>, V, R};

        {match, [E, V, R]} ->
            {E, V, R};

        _ ->
            throw(badarg)
    end;
str2evr(String) when is_list(String) ->
    str2evr(list_to_binary(String)).


% {{{ Internal functions
normalize(Props, OtherProps) ->
    Extracted = OtherProps ++ convert(conversions(), gb_trees:from_orddict(lists:keysort(1, Props)), []),
    case is_source(Extracted) of
        true ->
            {source, lists:keysort(1, [{sourcerpm, <<>>} | lists:keyreplace(arch, 1, Extracted, {arch, ?SRC_ARCH})])};

        false ->
            {binary, lists:keysort(1, Extracted)}
    end.

is_source(Props) ->
    case lists:keyfind('source', 1, Props) of
        false ->
            false;

        _ ->
            true
    end.

conversions() -> [
    {fun evr/3, [none]},
    {fun zip3/3, [
        {'changelog', {1080, 1081, 1082}, false},
        {'require', {1049, 1050, 1048}, true},
        {'provide', {1047, 1113, 1112}, true},
        {'conflict', {1054, 1055, 1053}, true},
        {'obsolete', {1090, 1115, 1114}, true}
    ]},
    {fun simple/3, [
        {1106, 'source'},
        {1000, 'name'},
        {1022, 'arch'},
        {1004, 'summary'},
        {1005, 'description'},
        {1015, 'packager'},
        {1020, 'url'},
        {1014, 'license'},
        {1011, 'vendor'},
        {1016, 'group'},
        {1007, 'buildhost'},
        {1006, 'buildtime'},
        {1044, 'sourcerpm'},
        {1046, 'archive_size'},
        {1009, 'size'}
    ]},
    {fun files/3, [none]}
].

convert([{Fun, Args} | Rest], Data, Acc) ->
    ActualFun = fun(Arg, AccIn) ->
        Fun(Arg, Data, AccIn)
    end,
    convert(Rest, Data, lists:foldl(ActualFun, Acc, Args));
convert([], _, Result) ->
    Result.

evr(_, Props, Acc) ->
    case lookup_many(Props, [{1003, 0}, {1001, none}, {1002, none}], []) of
        [Epoch, Version, Release] when Version /= none, Release /= none ->
            [{'evr', {Epoch, Version, Release}} | Acc];

        [Epoch, Version, Release] ->
            throw({bad_version, {Epoch, Version, Release}})
    end.

-define(MAGIC_DEFAULT, '$default$').

zip3({Tag, {K1, K2, K3}, Dependency}, Props, Acc) ->
    case lookup_many(Props, [{K1, ?MAGIC_DEFAULT}, {K2, ?MAGIC_DEFAULT}, {K3, ?MAGIC_DEFAULT}], []) of
        [First, Second, Third] when First /= ?MAGIC_DEFAULT, Second /= ?MAGIC_DEFAULT, Third /= ?MAGIC_DEFAULT ->
            Data = if
                Dependency ->
                    lists:filter(fun internal_filter/1,
                                 lists:zip3(First, Second,
                                            lists:map(fun flag2flags/1, Third)));

                true ->
                    lists:zip3(First, Second, Third)
            end,
            [{Tag, Data} | Acc];

        _ ->
            Acc
    end.

internal_filter({<<"rpmlib(", _/binary>>, _, _}) ->
    false;
internal_filter(_) ->
    true.

flag2flags(Flags) when is_integer(Flags) ->
    Text = case Flags band 15 of
        0 ->
            <<"">>;

        2 ->
            <<"LT">>;

        4 ->
            <<"GT">>;

        8 ->
            <<"EQ">>;

        10 ->
            <<"LE">>;

        12 ->
            <<"GE">>
    end,
    {Text, (Flags band 64) /= 0}.

simple({Key, Tag}, Props, Acc) ->
    case lookup(Props, Key, ?MAGIC_DEFAULT) of
        ?MAGIC_DEFAULT ->
            Acc;

        Value ->
            [{Tag, Value} | Acc]
    end.

files(_, Props, Acc) ->
    case lookup_many(Props, [{1118, ?MAGIC_DEFAULT}, {1116, ?MAGIC_DEFAULT}, {1117, ?MAGIC_DEFAULT}, {1037, ?MAGIC_DEFAULT}, {1030, ?MAGIC_DEFAULT}], []) of
        [Dirnames, DirIndices, Basenames, Flags, Modes] when Dirnames /= ?MAGIC_DEFAULT,
                                                             DirIndices /= ?MAGIC_DEFAULT,
                                                             Basenames /= ?MAGIC_DEFAULT ->
            [{files, lists:zip3(get_filenames(Dirnames, DirIndices, Basenames),
                                lists:map(fun fflag2flag/1, Flags),
                                lists:map(fun mode2mode/1, Modes))} | Acc];

        _ ->
            Acc
    end.

get_filenames(Dirnames, DirIndices, Basenames) ->
    Dirs = make_tree(Dirnames, gb_trees:empty(), 0),
    get_filenames(DirIndices, Basenames, Dirs, []).

get_filenames([Index | RIndex], [Name | RName], Dirs, Acc) ->
    case gb_trees:get(Index, Dirs) of
        <<>> ->
            get_filenames(RIndex, RName, Dirs, [Name | Acc]);

        Dir ->
            get_filenames(RIndex, RName, Dirs, [filename:join(Dir, Name) | Acc])
    end;
get_filenames([], [], _, Result) ->
    lists:reverse(Result).

make_tree([DirName | Rest], Tree, Index) ->
    make_tree(Rest, gb_trees:insert(Index, DirName, Tree), Index+1);
make_tree([], Tree, _) ->
    Tree.

mode2mode(Mode) when is_integer(Mode) ->
    case (Mode band 8#0170000) of
        8#0040000 ->
            dir;

        _ ->
            file
    end;
mode2mode(_) ->
    file.

fflag2flag(Flag) when is_integer(Flag) ->
    case (Flag band 64) of
        0 ->
            normal;

        _ ->
            ghost
    end;
fflag2flag(_) ->
    normal.

lookup(Tree, Key, Default) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Value} ->
            Value;

        _ ->
            Default
    end.

lookup_many(Tree, [{Key, Default} | Rest], Acc) ->
    lookup_many(Tree, Rest, [lookup(Tree, Key, Default) | Acc]);
lookup_many(_, [], Acc) ->
    lists:reverse(Acc).

% the code below is erlang implementation of what createrepo uses
get_header_range(FName) ->
    case file:open(FName, [read, binary, {read_ahead, 128000}]) of
        {ok, FD} ->
            file:position(FD, {bof, 104}),
            SigIndex = get_big_int(FD),
            SigData = get_big_int(FD),
            SigSize = SigData + SigIndex * 16,
            Start = 112 + SigSize + case SigSize rem 8 of
                0 ->
                    0;

                Other ->
                    8 - Other
            end,
            % seek past the magic number and reserved bytes
            file:position(FD, {bof, Start + 8}),
            HeaderIndex = get_big_int(FD),
            HeaderData = get_big_int(FD),
            % add 16 to the hdrsize to account for the 16 bytes of misc data b/t the
            End = Start + HeaderIndex * 16 + HeaderData + 16,
            file:close(FD),
            {ok, Start, End};

        Other ->
            Other
    end.

get_big_int(FD) ->
    {ok, Data} = file:read(FD, 4),
    <<Result:32/big-unsigned-integer>> = Data,
    Result.

get_info(FName) ->
    % This function seems to be introduced in a later release of Erlang.
    % file:read_file_info(FName, [{time, posix}]).
    case file:read_file_info(FName) of
        {ok, #file_info{size=Size, mtime=MTime}} ->
            % {Size, esums_helpers:posix_time(erlang:localtime_to_universaltime(MTime)),
            % ecrepo_utils:posix_time(MTime)}.
            {ok, Size, esums_helpers:posix_time(erlang:localtime_to_universaltime(MTime))};

        Other ->
            Other
    end.
% }}}

% {{{ EUnit tests
-ifdef(TEST).

str2evr_test() ->
    {"String to EpochVersionRevision conversion tests", [
        ?_assertEqual({<<"0">>, <<"1.0">>, <<"1">>}, ecrepo:str2evr("1.0-1")),
        ?_assertEqual({<<"0">>, <<"1.0">>, <<"1">>}, ecrepo:str2evr("0:1.0-1")),
        ?_assertEqual({<<"1">>, <<"1.0">>, <<"1">>}, ecrepo:str2evr("1:1.0-1")),
        ?_assertEqual({<<"1">>, <<"1.0">>, <<>>}, ecrepo:str2evr("1:1.0"))
    ]}.

-endif.
% }}}
