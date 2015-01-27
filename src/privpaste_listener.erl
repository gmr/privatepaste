%% ------------------------------------------------------------------
%% Cowboy Listener
%% ------------------------------------------------------------------
-module(privpaste_listener).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("privpaste.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    Dispatch = cowboy_router:compile(privpaste_routes:get()),
    cowboy:start_http(privpaste_cowboy_listener,
                      listener_count(),
                      [{port, port()}],
                      [{env, [{dispatch, Dispatch}]},
                       {timeout, timeout()},
                       {onresponse, fun on_response/4}]),
    {ok, {}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(privpaste_cowboy_listener).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Response handling behaviors
%% ------------------------------------------------------------------

on_response(Code, _Headers, _Body, Req) when is_integer(Code), Code >= 400 ->

    %% Need to maek this conditional on content-type

    lager:log(info, self(), "~p ~s ~s", [Code, cowboy_req:method(Req), cowboy_req:path(Req)]),
    Message = proplists:get_value(Code, ?STATUS_CODES, <<"Undefined Error Code">>),
    Opts = [{translation_fun, ?TRANSLATE,
            {locale, privpaste_util:get_language(Req)}],
    {ok, Body} = error_dtl:render([{code, integer_to_list(Code)},
                                   {message, Message}], Opts),
    cowboy_req:reply(Code,
                     [{<<"Content-Type">>, <<"text/html">>},
                      {<<"Content-Length">>, integer_to_list(byte_size(list_to_binary(Body)))}],
                      Body, Req);

on_response(Code, _Headers, _Body, Req) ->
    lager:log(info, self(), "~p ~s ~s", [Code, cowboy_req:method(Req), cowboy_req:path(Req)]),
    Req.

listener_count() ->
    privpaste_util:get_int_from_env("HTTP_LISTENER_COUNT", http_listener_count).

port() ->
    privpaste_util:get_int_from_env("HTTP_PORT", http_port).

timeout() ->
    privpaste_util:get_int_from_env("HTTP_TIMEOUT", http_timeout).