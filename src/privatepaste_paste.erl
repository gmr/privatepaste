%% ------------------------------------------------------------------
%% Paste Endpoint
%% ------------------------------------------------------------------
-module(privatepaste_paste).

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         get_html/2,
         get_json/2,
         resource_exists/2,
         save_paste/2,
         terminate/3]).

-include("privatepaste.hrl").

-define(ERROR, <<"error">>).
-define(NOT_FOUND, <<"Paste not found">>).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, save_paste}], Req, State}.

content_types_provided(Req, State) ->
    {[{?MIME_TYPE_JSON, get_json},
      {?MIME_TYPE_HTML, get_html}], Req, State}.

resource_exists(Req, _State) ->
	case cowboy_req:binding(paste_id, Req) of
		undefined ->
			{true, Req, index};
		PasteID ->
			case get_paste(cowboy_req:host(Req), PasteID) of
				not_found -> {false, Req, PasteID};
				_ ->         {true, Req, PasteID}
			end
	end.

terminate(_Reason, _Req, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Request Endpoints
%% ------------------------------------------------------------------

get_html(Req, State) ->
    case get_paste(cowboy_req:host(Req), cowboy_req:binding(paste_id, Req)) of
        undefined ->
            StatusCode = 404,
            Payload = jsx:encode([{?ERROR, ?NOT_FOUND}]);
        {ok, Paste} ->
            StatusCode = 200,
            Opts = [{translation_fun, ?TRANSLATE,
                    {locale, privatepaste_util:get_language(Req)}],
            {ok, Payload} = view_dtl:render([{paste, privatepaste_util:paste_to_proplist(Paste)}], Opts)
    end,
    Response = cowboy_req:reply(StatusCode, [{<<"content-type">>, <<"text/html">>}], Payload, Req),
    {stop, Response, State}.


get_json(Req, State) ->
    case get_paste(cowboy_req:host(Req), cowboy_req:binding(paste_id, Req)) of
        not_found ->
            StatusCode = 404,
            Payload = jsx:encode([{?ERROR, ?NOT_FOUND}]);
        {ok, Paste} ->
            StatusCode = 200,
            Data = privatepaste_util:paste_to_proplist(Paste),
            Payload = jsx:encode(proplists:delete(password, Data))
    end,
    Response = cowboy_req:reply(StatusCode, [{<<"content-type">>, <<"application/json">>}], Payload, Req),
    {stop, Response, State}.


save_paste(Req, State) ->
    {ok, Body, _} = cowboy_req:body(Req),
    Paste = privatepaste_util:proplist_to_paste(jsx:decode(Body)),
    case cowboy_req:method(Req) of
        <<"POST">> ->
            case Paste#paste.id of
                null ->
                    NewPaste = Paste#paste{id = privatepaste_util:new_id(), hostname = cowboy_req:host(Req)},

                    Result = insert_paste(NewPaste),
                    lager:log(info, self(), "insert_paste response: ~p", [Result]),
                    case Result of
                        ok ->
                            StatusCode = 201,
                            Payload = jsx:encode(privatepaste_util:paste_to_proplist(NewPaste));
                        Error ->
                            StatusCode = 500,
                            Payload = jsx:encode([{?ERROR, Error}])
                    end;
                _ ->
                    StatusCode = 500,
                    Payload = jsx:encode([{?ERROR, "Paste ID must not be specified"}])
            end;
        <<"PUT">> ->
            case Paste#paste.id of
                null ->
                    StatusCode = 404,
                    Payload = jsx:encode([{?ERROR, <<"PUT without Paste ID">>}]);
                _ ->
                    Result = update_paste(Paste),
                    lager:log(info, self(), "update_paste response: ~p", [Result]),
                    case Result of
                        {ok, Update} ->
                            StatusCode = 200,
                            Payload = jsx:encode(privatepaste_util:paste_to_proplist(Update));
                        Error ->
                            io:format("Error updating paste: ~p~n", [Error]),
                            StatusCode = 500,
                            Payload = jsx:encode([{?ERROR, Error}])
                    end
            end;
        _ ->
            StatusCode = 405,
            Payload = jsx:encode([{?ERROR, <<"Method not supported">>}])
    end,
    Response = cowboy_req:reply(StatusCode, [{<<"content-type">>, <<"application/json">>}], Payload, Req),
    {stop, Response, State}.

%% ------------------------------------------------------------------
%% Internal Methods
%% ------------------------------------------------------------------

get_paste(Hostname, ID) ->
    Pattern = #paste{hostname = Hostname, id = ID, _ = '_'},
    F = fun() ->
        mnesia:match_object(pastes, Pattern, read)
	end,
    case mnesia:transaction(F) of
        {atomic,[]}    -> not_found;
        {atomic,Paste} ->
            lager:log(info, self(), "Returned ~p records", [length(Paste)]),
            {ok, hd(Paste)}
    end.


insert_paste(Paste) ->
    lager:log(info, self(), "write_paste: ~p", [Paste]),
    F = fun() ->
        mnesia:write(pastes, Paste, write)
    end,
    mnesia:activity(transaction, F).


update_paste(Paste) ->
    F = fun() ->
        case mnesia:delete(pastes,  Paste#paste.id, write) of
            ok ->
                Update = Paste#paste{updated_at=calendar:universal_time(),
                                     revision=Paste#paste.revision + 1},
                Result = mnesia:write(pastes, Update, write),
                {Result, Update};
            Error ->
                lager:log(error, self(), "Erred when deleting previous record for ~s", [Paste#paste.id]),
                error
        end
    end,
    {atomic, Response} = mnesia:transaction(F),
    Response.
