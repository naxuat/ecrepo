-module(ecrepo_utils).
-vsn(?VERSION).

-include_lib("kernel/include/file.hrl").

-export([
    get/2,
    get_info/1
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
