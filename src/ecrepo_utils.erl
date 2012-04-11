-module(ecrepo_utils).
-vsn(?VERSION).

-export([
    get/2
]).

get(Defaults, PropList) ->
    {Result, _} = take(Defaults, PropList, []),
    Result.

take([{Key, Default} | Rest], Props, Result) ->
    case lists:keytake(Key, 1, Props) of
        {value, {_, Value}, OtherProps} ->
            take(Rest, OtherProps, [Value | Result]);

        false ->
            take(Rest, Props, [Default | Result])
    end;
take([], Props, Result) ->
    {lists:reverse(Result), Props}.
