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

-define(EMPTY, <<"">>).

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
    {StatusCode, Payload} = process_accept_request(Req),
    Req1 = cowboy_req:set_resp_body(Payload, Req),
    Req2 = cowboy_req:reply(StatusCode, Req1),
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

process_accept_request(Req) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            case privpaste_db:create_paste(cowboy_req:host(Req), get_submitted_data(Req)) of
                conflict       -> {409, ?EMPTY};
                {ok, Paste}    -> {201, jsx:encode(privpaste_util:paste_to_proplist(Paste))};
                {error, Error} -> {500, jsx:encode([{?ERROR, Error}])}
            end;
        <<"PUT">> ->
            case update_paste(Req) of
                conflict       -> {409, ?EMPTY};
                not_found      -> {404, ?EMPTY};
                unauthorized   -> {403, ?EMPTY};
                {ok, Paste}    -> {201, jsx:encode(privpaste_util:paste_to_proplist(Paste))};
                {error, Error} -> {500, jsx:encode([{?ERROR, Error}])}
            end;
        _ -> {405, <<"Method not supported">>}
    end.

process_get_html_request(Req) ->
    case get_paste(Req) of
        not_found    -> {404, ?EMPTY};
        unauthorized -> {403, ?EMPTY};
        {ok, Paste}  ->
            {ok, Payload} = view_dtl:render([{line_count, get_line_count(Paste#paste.content)},
                                             {modes, ?MODES},
                                             {paste, privpaste_util:paste_to_proplist(Paste)},
                                             {syntax, proplists:get_value(Paste#paste.syntax, ?MODES)},
                                             {ttl, proplists:get_value(Paste#paste.ttl, ?TTLS)}],
                                             privpaste_util:erlydtl_opts(Req)),
            {200, Payload}
    end.

process_get_json_request(Req) ->
    case get_paste(Req) of
        not_found    -> {404, ?EMPTY};
        unauthorized -> {403, ?EMPTY};
        {ok, Paste}  -> {200, jsx:encode(santize_paste(Paste))}
    end.

%% ------------------------------------------------------------------
%% Internal Methods
%% ------------------------------------------------------------------

check_authorization(Req, #paste{} = Paste) ->
    case Paste#paste.password of
        null        -> ok;
        Expectation ->
            Token = cowboy_req:binding(token, Req, <<"">>),
            case Token == Expectation of
                true  -> ok;
                false -> password_mismatch
            end
    end.

get_line_count(Content) ->
    length(lists:filter(fun(E) -> E == 10 end, binary_to_list(Content))).

get_paste(Req) ->
    case privpaste_db:get_paste(cowboy_req:host(Req), cowboy_req:binding(paste_id, Req)) of
        {ok, Paste} ->
            try
                ok = check_authorization(Req, Paste),
                {ok, Paste}
            catch
                error:{badmatch, password_mismatch} -> unauthorized
            end;
        not_found -> not_found
    end.

get_submitted_data(Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
    privpaste_util:proplist_to_paste(jsx:decode(Body)).

santize_paste(Paste) ->
    proplists:delete(password, privpaste_util:paste_to_proplist(Paste)).

%% TODO needs to handle authentication tied to owner
update_paste(Req) ->
    Hostname = cowboy_req:host(Req),
    Id = cowboy_req:binding(paste_id, Req),
    case privpaste_db:get_paste(Hostname, Id) of
        not_found -> not_found;
        {ok, _}   -> privpaste_db:update_paste(Hostname, Id, get_submitted_data(Req))
    end.
