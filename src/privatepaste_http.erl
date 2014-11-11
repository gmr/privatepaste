-module(privatepaste_http).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         language/1,
         code_change/3]).

-define(ACCEPTORS,  10).
-define(DEFAULT_LANGUAGE, "en").

-include("privatepaste_status_codes.hrl").

-record(state, {locales}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    Dispatch = cowboy_router:compile(routes()),
    cowboy:start_http(privatepaste_listener,
                      ?ACCEPTORS,
                      [{port, port()}],
                      [{env, [{dispatch, Dispatch}]},
                       {onresponse, fun on_response/4}]),
    {ok, #state{locales=gettext:all_lcs()}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(privatepaste_listener).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Private Function Definitions
%% ------------------------------------------------------------------

routes() ->
  %% {URIHost, list({URIPath, Handler, Opts})}
  [
    {'_', [
      {"/", privatepaste_home, []},
      {"/static/[...]", cowboy_static, {dir, [<<"static">>],
                                        [{mimetypes, cow_mimetypes, all},
                                         {dir_handler, directory_handler}]}}
    ]}
  ].


language(Req) ->
  {ok, Langs, _} = cowboy_req:parse_header(<<"accept-language">>, Req, ?DEFAULT_LANGUAGE),
  binary_to_list(lists:last(proplists:get_keys(Langs))).

%% ------------------------------------------------------------------
%% Hijack outbound responses to provide error pages
%% ------------------------------------------------------------------

on_response(Code, _Headers, _Body, Req) when is_integer(Code), Code >= 400 ->
  Message = proplists:get_value(Code, ?STATUS_CODES, <<"Undefined Error Code">>),
  Opts = [{translation_fun, fun(Key, Locale) -> gettext:key2str(Key, Locale) end},
          {locale, privatepaste_http:language(Req)}],
  {ok, Body} = error_page_dtl:render([{code, integer_to_list(Code)},
                                      {message, Message}], Opts),
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
  Req2;

on_response(_Code, _Headers, _Body, Req) ->
	Req.

port() ->
  case os:getenv("PORT") of
    false ->
      {ok, Port} = application:get_env(http_port),
      Port;
    Other ->
      list_to_integer(Other)
  end.
