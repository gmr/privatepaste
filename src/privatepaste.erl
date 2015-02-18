%% ------------------------------------------------------------------
%% PrivatePaste Core Application
%% ------------------------------------------------------------------
-module(privatepaste).

-export([start/0, start/2, stop/0, stop/1]).

-include("privpaste.hrl").

start() ->
    startApplication(),
    {ok, _Started} = application:ensure_all_started(privatepaste).

start(_StartType, _StartArgs) ->
    startApplication(),
    privpaste_sup:start_link().

stop() ->
    application:stop(privpaste).

stop(_State) ->
    ok.

startApplication() ->
    application:set_env(gettext, gettext_dir, "translations").
