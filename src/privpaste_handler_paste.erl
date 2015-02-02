%% ------------------------------------------------------------------
%% Paste Endpoints
%% ------------------------------------------------------------------
-module(privpaste_handler_paste).

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         get_html/2,
         get_json/2,
         terminate/3]).

-include("privpaste.hrl").


%% ------------------------------------------------------------------
%% Contract Entpoints for cowboy_rest
%% ------------------------------------------------------------------

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{?MIME_TYPE_JSON, get_json},
      {?MIME_TYPE_HTML, get_html}], Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Exposed Request Endpoints
%% ------------------------------------------------------------------

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    Decoded = privpaste_paste:to_record(jsx:decode(Body)),
    {IP, _} = cowboy_req:peer(Req),
    {ok, MimeType} = emagic:from_buffer(Decoded#paste.content),
    Record = Decoded#paste{remote_ip=IP,
                           request_headers=cowboy_req:headers(Req),
                           mime_type=MimeType},
    {StatusCode, Payload} = process_accept_request(cowboy_req:method(Req1),
                                                   cowboy_req:host(Req1),
                                                   cowboy_req:binding(paste_id, Req1, null),
                                                   Record),
    Req2 = cowboy_req:reply(StatusCode, [?CONTENT_TYPE_JSON], Payload, Req1),
    {stop, Req2, State}.

get_html(Req, State) ->
    {StatusCode, Payload} = process_get_html_request(Req),
    Req1 = cowboy_req:set_resp_body(Payload, Req),
    Req2 = cowboy_req:reply(StatusCode, Req1),
    {stop, Req2, State}.

get_json(Req, State) ->
    {StatusCode, Payload} = process_get_json_request(Req),
    Req1 = cowboy_req:set_resp_body(Payload, Req),
    Req2 = cowboy_req:reply(StatusCode, Req1),
    {stop, Req2, State}.

%% ------------------------------------------------------------------
%% Internal Request Processors
%% ------------------------------------------------------------------

process_accept_request(Method, Hostname, null, Data) when Method == <<"POST">> ->
    case privpaste_db:create_paste(Hostname, Data) of
        conflict       -> {409, ?EMPTY};
        {ok, Paste}    ->
            Sanitized = privpaste_paste:sanitize(Paste),
            {201, jsx:encode(Sanitized)};
        {error, Error} -> {500, jsx:encode([{?ERROR, Error}])}
    end;

process_accept_request(Method, _, _, _) when Method == <<"POST">> ->
    {409, ?EMPTY};

%% TODO needs to handle authentication tied to owner
process_accept_request(Method, Hostname, Id, Data) when Method == <<"PUT">> ->
    case privpaste_db:update_paste(Hostname, Id, Data) of
        conflict       -> {409, ?EMPTY};
        not_found      -> {404, ?EMPTY};
        unauthorized   -> {403, ?EMPTY};
        {ok, Paste}    -> {201, jsx:encode(privpaste_paste:to_proplist(Paste))};
        {error, Error} -> {500, jsx:encode([{?ERROR, Error}])}
    end;

process_accept_request(_, _, _, _) ->
    {405, ?EMPTY}.

process_get_html_request(Req) ->
    case privpaste_paste:get(Req) of
        not_found    -> {404, ?EMPTY};
        unauthorized -> {403, ?EMPTY};
        {ok, Paste}  ->
            {ok, Payload} = view_dtl:render([{line_count, get_line_count(Paste#paste.content)},
                                             {modes, ?MODES},
                                             {paste, privpaste_paste:to_proplist(Paste)},
                                             {syntax, proplists:get_value(Paste#paste.syntax, ?MODES)},
                                             {ttl, proplists:get_value(Paste#paste.ttl, ?TTLS)}],
                                             privpaste_util:erlydtl_opts(Req)),
            {200, list_to_binary(Payload)}
    end.

process_get_json_request(Req) ->
    case privpaste_paste:get(Req) of
        not_found    -> {404, ?EMPTY};
        unauthorized -> {403, ?EMPTY};
        {ok, Paste}  -> {200, jsx:encode(privpaste_paste:sanitize(Paste))}
    end.

%% ------------------------------------------------------------------
%% Internal Methods
%% ------------------------------------------------------------------

get_line_count(Content) ->
    length(lists:filter(fun(E) -> E == 10 end, binary_to_list(Content))) + 1.
