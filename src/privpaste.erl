-module(privpaste).

-export([start/0, start/2, stop/0, stop/1]).

-include("privpaste.hrl").

start() ->
    startApplication(),
    {ok, _Started} = application:ensure_all_started(privpaste).

start(_StartType, _StartArgs) ->
    startApplication(),
    privpaste_sup:start_link().

stop() ->
    application:stop(privpaste).

stop(_State) ->
    ok.

startApplication() ->
    application:set_env(gettext, gettext_dir, "translations"),
    random:seed(erlang:now()),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(pastes, [{attributes, record_info(fields, paste)},
                                 {disc_copies, [node()]},
                                 {record_name, paste},
                                 {type, set}]).
