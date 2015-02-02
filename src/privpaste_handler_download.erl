%% ------------------------------------------------------------------
%% Download Page Endpoint
%% ------------------------------------------------------------------
-module(privpaste_handler_download).

%% API
-export([init/2,
         allowed_methods/2,
         content_types_provided/2,
         terminate/3,
         get/2]).

-include("privpaste.hrl").

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"html">>, []}, get}], Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

get(Req, State) ->
    {StatusCode, MimeType, Payload} = process(Req),
    lager:info("Responding: (~p, ~p)~n~n~p", [StatusCode, MimeType, Payload]),
    Req1 = cowboy_req:reply(StatusCode,
                            [{?CONTENT_TYPE, MimeType},
                             {?CONTENT_LENGTH, privpaste_util:get_body_size(Payload)}],
                            Payload,
                            Req),
    {stop, Req1, State}.

process(Req) ->
    case privpaste_paste:get(Req) of
        not_found    -> {404, ?CONTENT_TYPE_HTML, ?EMPTY};
        unauthorized -> {403, ?CONTENT_TYPE_HTML, ?EMPTY};
        {ok, Paste}  -> {200, Paste#paste.mime_type, Paste#paste.content}
    end.

