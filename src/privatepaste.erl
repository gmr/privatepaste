-module(privatepaste).

-export([start/0, start/2, stop/0, stop/1]).

start() ->
    lager:start(),
    application:set_env(gettext, gettext_dir, "translations"),
    {ok, _Started} = application:ensure_all_started(privatepaste).

start(_StartType, _StartArgs) ->
    lager:start(),
    application:set_env(gettext, gettext_dir, "translations"),
    privatepaste_sup:start_link().

stop() ->
    application:stop(privatepaste).

stop(_State) ->
    ok.
