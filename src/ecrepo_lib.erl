-module(ecrepo_lib).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    header/1,
    compare/2,
    tag2name/1,
    name2tag/1,
    quote/1
]).

-on_load(init/0).

-define(SO_NAME, "ecrepo_lib_nif").
-define(NIF_STUB, nif_stub_error(?LINE)).

init() ->
    PrivDir = case code:priv_dir(?SO_NAME) of
        {error, bad_name} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, ?SO_NAME), 0).

header(_) ->
    ?NIF_STUB.

compare(_, _) ->
    ?NIF_STUB.

tag2name(_) ->
    ?NIF_STUB.

name2tag(_) ->
    ?NIF_STUB.

quote(_) ->
    ?NIF_STUB.

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

% {{{ EUnit tests
-ifdef(TEST).

%% this test just checks that NIF can be loaded and there are no unresolved
%% sybmols.
load_test() ->
    ok.

compare_test() -> 
    {"Version comparison tests", [
        ?_assertEqual(0, compare("1.0", "1.0")),
        ?_assertEqual(0, compare("0:1.0", "1.0")),
        ?_assertEqual(0, compare("0:1.0", "0:1.0")),
        ?_assertEqual(-1, compare("0:1.0", "1:1.0")),
        ?_assertEqual(-1, compare("1.0", "2.0")),
        ?_assertEqual(-1, compare("2.0", "2.0.1")),
        ?_assertEqual(1, compare("2.0.1a", "2.0.1")),
        ?_assertEqual(-1, compare("5.5p1", "5.5p2")),
        ?_assertEqual(1, compare("5.6p1", "5.5p2")),
        ?_assertEqual(-1, compare("10a2", "10b2")),
        ?_assertEqual(-1, compare("1.0a", "1.0aa")),
        ?_assertEqual(1, compare("6.0rc1", "6.0")),
        ?_assertEqual(1, compare("6.0.rc1", "6.0")),
        ?_assertEqual(-1, compare("10.1.1.0001", "10.1.1.39")),
        ?_assertEqual(1, compare("5.0", "4.999.9")),
        ?_assertEqual(-1, compare("2.030", "2.031")),
        ?_assertEqual(1, compare("20101122", "20101121")),
        ?_assertEqual(0, compare("2.0", "2_0"))
    ]}.
-endif.
% }}}
