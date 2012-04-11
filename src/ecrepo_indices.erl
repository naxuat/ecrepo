-module(ecrepo_indices).

-export([
    repomd/3,
    indices/1
]).

-include("ecrepo_common.hrl").

-define(BASE_NS, "http://linux.duke.edu/metadata/").
-define(REPO_NS, <<?BASE_NS "repo">>).
-define(COMMON_NS, <<?BASE_NS "common">>).
-define(FILELISTS_NS, <<?BASE_NS "filelists">>).
-define(OTHER_NS, <<?BASE_NS "other">>).
-define(RPM_NS, <<?BASE_NS "rpm">>).

-define(RPM, <<"rpm">>).

-define(REPOMD_DIR, "repodata").
-define(REPOMD, <<"repomd.xml">>).
-define(SUFFIX, ".xml.gz").

-define(TBD, <<"tbd">>).

% {{{ Helper macros for frequently used operations
-define(L(Key, Tree), lookup(Key, Tree)).
-define(E(Name, Attrs, Content), {Name, Attrs, Content}).
-define(F(Format, Args), ecrepo_xml:format_to_bin(Format, Args)).
% }}}

% the number of packages is required as processing a long list might take too
% much time
repomd(OutDir, Count, Packages) when is_binary(OutDir) ->
    RepoMDDir = filename:join(OutDir, <<?REPOMD_DIR>>),
    IndexName = filename:join(RepoMDDir, ?REPOMD),
    ok = filelib:ensure_dir(IndexName),
    {Primary, FileLists, Other} = lists:unzip3(Packages),
    Index = ecrepo_xml:document({
        repomd, [
            {xmlns, ?REPO_NS},
            {{xmlns, rpm}, ?RPM_NS}
        ], [
            {revision, esums_helpers:utc()},
            entry(<<"primary">>, RepoMDDir, ?COMMON_NS, Count, Primary),
            entry(<<"filelists">>, RepoMDDir, ?FILELISTS_NS, Count, FileLists),
            entry(<<"other">>, RepoMDDir, ?OTHER_NS, Count, Other)
        ]
    }),
    file:write_file(IndexName, Index);
repomd(OutDir, Count, Packages) when is_list(OutDir) ->
    repomd(list_to_binary(OutDir), Count, Packages).

indices(Package) when is_list(Package) ->
    Tree = make_tree(Package, gb_trees:empty()),
    {ecrepo_xml:elem(entry(primary(), Tree)),
    ecrepo_xml:elem(entry(filelist(),Tree)),
    ecrepo_xml:elem(entry(other(), Tree))};
indices({_Kind, Package}) ->
    indices(Package).

% Below are internal functions

entry(Type, OutDir, NS, Count, Packages) ->
    Document = ecrepo_xml:document(?E(metadata, [
        {xmlns, NS},
        {{xmlns, rpm}, ?RPM_NS},
        {packages, Count}
    ], Packages)),
    Hash = esums_pair:complete(esums_pair:open(filename:join([OutDir, Type]), esums_file, false),
                               Document),
    {_, _, OpenSums} = esums_pair:info(Hash, plain),
    {_, MTime, Sums} = esums_pair:info(Hash, gzipped),
    [OpenSum, OpenSize] = ecrepo_utils:get([{sha256, oops}, {size, oops}], OpenSums),
    [Sum, Size] = ecrepo_utils:get([{sha256, oops}, {size, oops}], Sums),
    ?E(data, [{type, Type}], [
        ?E(checksum, [{type, <<"sha256">>}], esums:format(sha256, Sum)),
        ?E('open-checksum', [{type, <<"sha256">>}], esums:format(sha256, OpenSum)),
        ?E(location, [{href, <<?REPOMD_DIR, "/", Type/binary, ?SUFFIX>>}], []),
        ?E(timestamp, [], MTime),
        ?E(size, [], Size),
        ?E('open-size', [], OpenSize)]).

primary() -> {
    {package, [{type, ?RPM}]}, [
        {name, name},
        {arch, arch},
        {version, fun version/1},
        {checksum, fun checksum/1},
        {summary, summary},
        {description, description},
        {packager, packager},
        {url, url},
        {time, fun fulltime/1},
        {size, fun fullsize/1},
        {location, fun location/1},
        {format, fun format/1}
    ]
}.

format_entry() -> {
    {format, []}, [
        {{rpm, license}, license},
        {{rpm, vendor}, vendor},
        {{rpm, group}, group},
        {{rpm, buildhost}, buildhost},
        {{rpm, sourcerpm}, sourcerpm},
        {{rpm, 'header-range'}, fun header_range/1},
        {ignore, fun provides/1},
        {ignore, fun requires/1},
        {ignore, fun conflicts/1},
        {ignore, fun obsoletes/1},
        {ignore, fun useful_files/1}
    ]
}.

filelist() -> {
    {package, [
        {name, name},
        {arch, arch}
    ]}, fun files/1
}.

other() -> {
    {package, [
        {name, name},
        {arch, arch}
    ]}, fun changelog/1
}.

entry({{Top, Attrs}, Body}, Package) when is_list(Body) ->
    ?E(Top, extract(Attrs, Package), extract(Body, Package));
entry({{Top, Attrs}, Fun}, Package) ->
    ?E(Top, extract(Attrs, Package), Fun(Package)).

extract([{Name, Value} | Rest], Package) when is_binary(Value) ->
    [{Name, Value} | extract(Rest, Package)];
extract([{Name, Key} | Rest], Package) when is_atom(Key) ->
    [{Name, ?L(Key, Package)} | extract(Rest, Package)];
extract([{Name, Fun} | Rest], Package) when is_function(Fun) ->
    handle_fun(Name, Fun(Package), extract(Rest, Package));
extract([], _) ->
    [].

handle_fun(_, none, Tail) ->
    Tail;
handle_fun(_, {append, Content}, Tail) ->
    Content ++ Tail;
handle_fun(Name, {Attrs, Content}, Tail) ->
    [?E(Name, Attrs, Content) | Tail];
handle_fun(_, {_, _, _}=Result, Tail) ->
    [Result | Tail];
handle_fun(Name, Content, Tail) ->
    [?E(Name, [], Content) | Tail].

format(Package) ->
    entry(format_entry(), Package).

header_range(Package) -> {
    % using apostrophes, as 'end' is a reserved word
    [{start, ?L(header_start, Package)}, {'end', ?L(header_end, Package)}],
    []
}.

provides(Package) ->
    deps2xml(provide, provides, false, Package).

requires(Package) ->
    deps2xml(require, requires, true, Package).

conflicts(Package) ->
    deps2xml(conflict, conflicts, false, Package).

obsoletes(Package) ->
    deps2xml(obsolete, obsoletes, false, Package).

make_tree([{Key, Value} | Rest], Tree) ->
    make_tree(Rest, gb_trees:insert(Key, Value, Tree));
make_tree([], Tree) ->
    Tree.

lookup(Key, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Value} ->
            Value;

        _ ->
            throw({not_found, Key})
    end.

version(Package) ->
    {Epoch, Version, Release} = ?L(evr, Package),
    {[
        {epoch, Epoch},
        {ver, Version},
        {rel, Release}
    ], <<>>}.

checksum(Package) ->
    {[{type, <<"sha256">>}, {pkgid, <<"YES">>}], ?L(sha256, Package)}.

fulltime(Package) -> {
    [{file, ?L(filetime, Package)}, {build, ?L(buildtime, Package)}],
    []
}.

fullsize(Package) -> {
    [{package, ?L(filesize, Package)}, {installed, ?L(size, Package)}, {archive, ?L(archive_size, Package)}],
    []
}.

location(Package) -> {
    [{href, ?L(location, Package)}],
    []
}.

useful_files(Package) ->
    {ok, FileMP} = re:compile(<<"^(.*bin\/.*|\/etc\/.*|\/usr\/lib\/sendmail)$">>),
    {ok, DirMP} = re:compile(<<"^(.*bin\/.*|\/etc\/.*)$">>),
    {Files, _, _} = lists:foldl(fun useful_files/2, {[], FileMP, DirMP}, ?L(files, Package)),
    if
        Files /= [] ->
            {append, Files};

        true ->
            none
    end.

files(Package) ->
    lists:map(fun file2file/1, ?L(files, Package)).

useful_files({Name, ghost, _}, {Acc, MP, _}=State) ->
    case re:run(Name, MP) of
        nomatch ->
            State;

        _ ->
            setelement(1, State, [file2file(Name, [{type, <<"ghost">>}]) | Acc])
    end;
useful_files({Name, _, dir}, {Acc, _, MP}=State) ->
    case re:run(Name, MP) of
        nomatch ->
            State;

        _ ->
            setelement(1, State, [file2file(Name, [{type, <<"dir">>}]) | Acc])
    end;
useful_files({Name, _, _}, {Acc, MP, _}=State) ->
    case re:run(Name, MP) of
        nomatch ->
            State;

        _ ->
            setelement(1, State, [file2file(Name, []) | Acc])
    end.

file2file({Name, ghost, _}) ->
    file2file(Name, [{type, <<"ghost">>}]);
file2file({Name, _, dir}) ->
    file2file(Name, [{type, <<"dir">>}]);
file2file({Name, _, _}) ->
    file2file(Name, []).

file2file(Name, Attrs) ->
    ?E(file, Attrs, Name).

changelog(Package) ->
    chlog2chlog(?L(changelog, Package), 10, []).

chlog2chlog([{Date, Author, Text} | Rest], Left, Acc) when Left > 0 ->
    chlog2chlog(Rest, Left-1,
                [?E(changelog, [{author, Author}, {date, Date}], Text) | Acc]);
chlog2chlog(_, _, Result) ->
    Result.

deps2xml(Key, Name, IncPre, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Deps} ->
            ?E({rpm, Name}, [], format_deps(Deps, IncPre, []));

        Other ->
            Other
    end.

format_deps([{Dep, Id, {Flag, Pre}} | Rest], IncPre, Acc) ->
    format_deps(Rest, IncPre, [?E({rpm, entry}, [{name, Dep} | flags(Flag, Id, pre(Pre, IncPre))], []) | Acc]);
format_deps([], _, Result) ->
    % we do not care about the order in which dependencies are shown
    Result.

flags(<<>>, <<>>, PreReq) ->
    PreReq;
flags(Flags, Id, PreReq) when Id /= <<>> ->
    [{flags, Flags} | case ecrepo:str2evr(Id) of
        {Epoch, Version, <<>>} ->
            [{epoch, Epoch}, {ver, Version} | PreReq];

        {Epoch, Version, Release} ->
            [{epoch, Epoch}, {ver, Version}, {rel, Release} | PreReq]
    end].

pre(_, false) ->
    [];
pre(true, true) ->
    [{pre, <<"1">>}];
pre(false, true) ->
    [{pre, <<"0">>}].
