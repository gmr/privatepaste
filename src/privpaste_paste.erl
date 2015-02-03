%% ------------------------------------------------------------------
%% Common functionality for interacting with pastes
%% ------------------------------------------------------------------
-module(privpaste_paste).

-include("privpaste.hrl").

%% API
-export([get/1,
         sanitize/1,
         to_proplist/1,
         to_record/1]).

get(Req) ->
    Hostname = cowboy_req:host(Req),
    Id = cowboy_req:binding(paste_id, Req),
    case privpaste_db:get_paste(Hostname, Id) of
        {ok, Paste} ->
            try
                ok = check_authorization(Req, Paste),
                privpaste_db:increment_paste_views(Hostname, Id),
                {ok, Paste}
            catch
                error:{badmatch, password_mismatch} -> unauthorized
            end;
        not_found -> not_found
    end.

sanitize(Paste) ->
    Rec1 = proplists:delete(password, to_proplist(Paste)),
    Rec2 = proplists:delete(remote_ip, Rec1),
    Rec3 = proplists:delete(request_headers, Rec2),
    proplists:delete(owner, Rec3).

to_proplist(#paste{} = Record) ->
    Data = lists:zip(record_info(fields, paste), tl(tuple_to_list(Record))),
    Update = maybe_convert_iso8601_to_timetuple(created_at, Data),
    maybe_convert_iso8601_to_timetuple(updated_at, Update).

to_record(Data) ->
    #paste{id = privpaste_util:get_binary_from_proplist(id, Data, null),
           hostname = privpaste_util:get_binary_from_proplist(hostname, Data),
           owner = privpaste_util:get_binary_from_proplist(owner, Data),
           created_at = maybe_convert_timetuple_to_iso8601(created_at, Data),
           updated_at = maybe_convert_timetuple_to_iso8601(updated_at, Data),
           revision = privpaste_util:get_int_from_proplist(revision, Data),
           ttl = privpaste_util:get_int_from_proplist(ttl, Data),
           password = privpaste_util:get_binary_from_proplist(password, Data),
           views = privpaste_util:get_int_from_proplist(views, Data),
           syntax = privpaste_util:get_binary_from_proplist(syntax, Data),
           line_numbers = privpaste_util:get_atom_from_proplist(line_numbers, Data, false),
           mime_type = privpaste_util:get_binary_from_proplist(mime_type, Data),
           content = privpaste_util:get_binary_from_proplist(content, Data)}.

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

maybe_convert_iso8601_to_timetuple(Key, Data) ->
    case proplists:get_value(Key, Data, null) of
        null   -> Data;
        Value  ->
            New = proplists:delete(Key, Data),
            Formatted = iso8601:format(Value),
            lists:append(New, [{Key, Formatted}])
    end.

maybe_convert_timetuple_to_iso8601(Key, Data) ->
    case privpaste_util:get_binary_from_proplist(Key, Data, null) of
        null      -> null;
        Date      -> iso8601:parse(Date)
    end.


