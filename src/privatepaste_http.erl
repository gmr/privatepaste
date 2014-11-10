-module(privatepaste_http).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(ACCEPTORS,  100).

-record(state, {}).

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
                      [{env, [{dispatch, Dispatch}]}]),
    {ok, #state{}}.

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
      %%%{"/_", privatepaste_404, []}
    ]}
  ].

port() ->
  case os:getenv("PORT") of
    false ->
      {ok, Port} = application:get_env(http_port),
      Port;
    Other ->
      list_to_integer(Other)
  end.
