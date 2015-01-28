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
    Dispatch = privpaste_routes:get(),
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

on_response(Code, Headers, _Body, Req) when is_integer(Code), Code >= 400 ->
    lager:log(info, self(), "~p ~s ~s", [Code, cowboy_req:method(Req), cowboy_req:path(Req)]),
    [_, _, Subtype, _] = proplists:get_value(?CONTENT_TYPE, Headers),
    {Headers1, Body} = get_error_response(Code, Subtype, Req),
    Req1 = cowboy_req:reply(Code, merge_headers(Headers, Headers1), Body, Req),
    Req1;

on_response(Code, _Headers, _Body, Req) ->
    lager:log(info, self(), "~p ~s ~s", [Code, cowboy_req:method(Req), cowboy_req:path(Req)]),
    Req.

%% ------------------------------------------------------------------
%% Internal methods
%% ------------------------------------------------------------------
get_body_size(Body) when is_binary(Body) ->
    integer_to_list(byte_size(Body));

get_body_size(Body) when is_list(Body) ->
    integer_to_list(byte_size(list_to_binary(Body))).

get_error_response(Code, Subtype, Req) ->
    case Subtype of
        <<"json">> ->
            Body = jsx:encode([{?ERROR, proplists:get_value(Code, ?STATUS_CODES)}]),
            Headers = [{?CONTENT_TYPE, ?MIME_TYPE_JSON},
                       {?CONTENT_LENGTH, get_body_size(Body)}],
            {Headers, Body};
        _ ->
            {ok, Body} = error_dtl:render([{code, integer_to_list(Code)},
                                           {message, proplists:get_value(Code, ?STATUS_CODES)}],
                                           privpaste_util:erlydtl_opts(Req)),
            Headers = [{?CONTENT_TYPE, ?MIME_TYPE_HTML},
                       {?CONTENT_LENGTH, get_body_size(Body)}],
            {Headers, Body}
    end.

merge_headers(Headers1, Headers2) ->
     orddict:merge(fun(_, X, Y) -> X, Y end,
                   orddict:from_list(Headers1),
                   orddict:from_list(Headers2)).

listener_count() ->
    privpaste_util:get_int_from_env("HTTP_LISTENER_COUNT", http_listener_count).

port() ->
    privpaste_util:get_int_from_env("HTTP_PORT", http_port).

timeout() ->
    privpaste_util:get_int_from_env("HTTP_TIMEOUT", http_timeout).
