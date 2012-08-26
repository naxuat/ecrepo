-module(ecrepo_utils).
-vsn(?VERSION).

-include_lib("kernel/include/file.hrl").

-export([
    get/2,
    get_info/1,
    abspath/1,
    simple_join/2,
    copy/3
]).

get(Defaults, PropList) ->
    {Result, _} = take(Defaults, PropList, []),
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

% Unfortunately, filename:absname/1 and filename:join/* do not have
% intermediate . as I'd like them to.
% Until I find a solution, the hack below will be used.
abspath(Path) ->
    case filelib:is_dir(Path) of
        true ->
            get_abspath(Path);

        false ->
            filename:join(get_abspath(filename:dirname(Path)),
                          filename:basename(Path))
    end.

simple_join(DirName, FileName) ->
    <<DirName/binary, $/, FileName/binary>>.

copy(Source, Dest, false) ->
    file:copy(Source, Dest);
copy(Source, Dest, true) ->
    file:rename(Source, Dest).

%% Internal functions

take([{Key, Default} | Rest], Props, Result) ->
    case lists:keytake(Key, 1, Props) of
        {value, {_, Value}, OtherProps} ->
            take(Rest, OtherProps, [Value | Result]);

        false ->
            take(Rest, Props, [Default | Result])
    end;
take([], Props, Result) ->
    {lists:reverse(Result), Props}.

get_abspath(Path) ->
    {ok, OldCWD} = file:get_cwd(),
    file:set_cwd(Path),
    {ok, Result} = file:get_cwd(),
    file:set_cwd(OldCWD),
    Result.
