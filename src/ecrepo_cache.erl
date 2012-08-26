-module(ecrepo_cache).

-export([
    load/1,
    save/1,
    install/5,
    refresh/1,
    layouts/0
]).

-define(CACHE_DIR, ".ecrepo/cache").
-define(MAX_DELETES, 10).

-record(ecrepo_cache, {
    dir :: binary(),
    dirlen :: integer(),
    cache_name :: binary(),
    changed = false :: boolean(),
    new :: gb_tree(),
    deletes = 0 :: integer(),
    old :: gb_tree()
}).

load(DestDir) ->
    DirName = iolist_to_binary(ecrepo_utils:abspath(DestDir)),
    CacheName = cache_name(DirName),
    new(DirName, CacheName, filelib:is_regular(CacheName)).

save(#ecrepo_cache{dir=DirName, cache_name=CacheName, changed=true, new=Cache}) ->
    generate(DirName, Cache),
    ok = file:write_file(CacheName, term_to_binary(Cache, [{compressed, 9}]));
save(_) ->
    ok.

install(OrigName, Schema, RPM, Delete, #ecrepo_cache{dir=DirName}=State) ->
    Location = get_location(Schema, RPM),
    FullName = ecrepo_utils:simple_join(DirName, Location),
    ok = filelib:ensure_dir(FullName),
    ecrepo_utils:copy(OrigName, FullName, Delete),
    update_caches(FullName, Location, State).

refresh(#ecrepo_cache{dir=DirName}=State) ->
    NewState = filelib:fold_files(DirName, "^.*\.rpm$", true, fun refresh_file/2, State),
    case gb_trees:is_empty(NewState#ecrepo_cache.old) of
        true ->
            NewState;

        false ->
            NewState#ecrepo_cache{changed=true, old=gb_trees:empty()}
    end.

layouts() -> [
    {arch, "Based on package architecture"},
    {plain, "All packages on the top level (default)"},
    {srpm, "Based on source package name"}
].

% Internal Functions

new(DirName, CacheName, false) ->
    new(DirName, CacheName, true, gb_trees:empty());
new(DirName, CacheName, true) ->
    {ok, Data} = file:read_file(CacheName),
    new(DirName, CacheName, false, binary_to_term(Data)).

new(DirName, CacheName, Changed, Cache) ->
    #ecrepo_cache{dir=DirName, dirlen=byte_size(DirName)+1,
                  cache_name=CacheName,
                  changed=Changed,
                  new=gb_trees:empty(),
                  old=Cache}.

cache_name(DirName) ->
    FullName = filename:join([os:getenv("HOME"),
                              ?CACHE_DIR,
                              esums:format(md5, esums:simple(md5, DirName))]),
    ok = filelib:ensure_dir(FullName),
    FullName.

generate(DirName, Cache) ->
    Packages = pre_process(gb_trees:next(gb_trees:iterator(Cache)), []),
    ecrepo_indices:repomd(DirName, gb_trees:size(Cache), Packages).

pre_process({_, {_, _, RPM}, Iter}, Acc) ->
    pre_process(gb_trees:next(Iter), [ecrepo_indices:indices(RPM) | Acc]);
pre_process(none, Acc) ->
    Acc.

get_location(Schema, RPM) ->
    Name = ecrepo:name(RPM, file),
    get_location(Schema, Name, RPM).

get_location(arch, Name, RPM) ->
    {arch, Arch} = lists:keyfind(arch, 1, RPM),
    ecrepo_utils:simple_join(Arch, Name);
get_location(plain, Name, _RPM) ->
    Name;
get_location(srpm, Name, RPM) ->
    {sourcerpm, SourceRPM} = lists:keyfind(sourcerpm, 1, RPM),
    Dir = case SourceRPM of
        <<>> ->
            {name, PackageName} = lists:keyfind(name, 1, RPM),
            PackageName;

        _ ->
            source_name(SourceRPM)
    end,
    ecrepo_utils:simple_join(Dir, Name).

-define(RE_PATTERN, <<"^(.+)-([^-]+)-([^-]+)\.([^.]+)\.rpm$">>).
-define(RE_OPTIONS, [ungreedy, {capture, all_but_first, binary}]).

source_name(What) ->
    case  re:run(What, ?RE_PATTERN, ?RE_OPTIONS) of
        {match, [Name, _, _, _]} ->
            Name;

        _ ->
            What
    end.

refresh_file(FileName, #ecrepo_cache{dirlen=DirLen, new=New}=State) ->
    <<_:DirLen/binary, RelativeName/binary>> = FileName,
    case gb_trees:lookup(RelativeName, New) of
        none ->
            update_caches(FileName, RelativeName, State);

        _ ->
            State
    end.

update_caches(FileName, RelativeName, #ecrepo_cache{new=New, changed=OldChanged, deletes=Deletes, old=Old}=State) ->
    case gb_trees:lookup(RelativeName, Old) of
        {value, Value} ->
            {Changed, Info} = validate(FileName, RelativeName, Value),
            UpdatedNew = gb_trees:insert(RelativeName, Info, New),
            {UpdatedOld, UpdatedDeletes} = case {gb_trees:delete(RelativeName, Old), Deletes} of
                {Tempo, ?MAX_DELETES} ->
                    {gb_trees:balance(Tempo), 0};

                Other ->
                    Other
            end,
            State#ecrepo_cache{new=UpdatedNew, changed=Changed or OldChanged, deletes=UpdatedDeletes, old=UpdatedOld};

        _ ->
            UpdatedNew = gb_trees:insert(RelativeName, get_new(FileName, RelativeName), New),
            State#ecrepo_cache{new=UpdatedNew, changed=true}
    end.

validate(FileName, RelativeName, {MTime, Size, _}=OldValue) ->
    case ecrepo_utils:get_info(FileName) of
        {ok, Size, MTime} ->
            {false, OldValue};

        _ ->
            {true, get_new(FileName, RelativeName)}
    end.

get_new(FileName, RelativeName) ->
    {ok, {_, RPM}} = ecrepo:read(FileName),
    extract(RPM, RelativeName).

extract(RPM, RelativeName) ->
    {filetime, MTime} = lists:keyfind(filetime, 1, RPM),
    {filesize, Size} = lists:keyfind(filesize, 1, RPM),
    {MTime, Size, lists:keyreplace(location, 1, RPM, {location, RelativeName})}.
