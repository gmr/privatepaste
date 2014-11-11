-module(privatepaste_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APPS, [crypto, ranch, cowlib, cowboy, compiler, syntax_tools, gettext, erlydtl]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:set_env(gettext, gettext_dir, "translations"),
    ok = ensure_started(?APPS),
    privatepaste_sup:start_link().

stop(_State) ->
    stop_apps(?APPS),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	application:stop(App),
	stop_apps(Apps).
