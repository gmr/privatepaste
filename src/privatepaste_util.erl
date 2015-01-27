-module(privatepaste_util).

-export([get_atom_from_proplist/2,
         get_atom_from_proplist/3,
         get_binary_from_proplist/2,
         get_binary_from_proplist/3,
         get_int_from_proplist/2,
         get_int_from_proplist/3,
         get_int_from_env/2,
         get_language/1,
         is_expired/1,
         new_id/0,
         paste_to_proplist/1,
         proplist_to_paste/1]).

-include("privatepaste.hrl").

-define(ID_CHARS, "0123456789ABCDEF").
-define(ID_LENGTH, 10).

get_int_from_env(OS, App) ->
    case os:getenv(OS) of
        false ->
            {ok, Port} = application:get_env(App),
            Port;
        Other ->
            list_to_integer(Other)
    end.

get_language(Req) ->
    case cowboy_req:parse_header(<<"accept-language">>, Req) of
        undefined -> list_to_binary(gettext:default_lang());
        Accept ->
            Ls = [binary_to_list(L) || {L, _} <- Accept],
            All = gettext:all_lcs(),
            case [L || L <- Ls, lists:member(L, All)] of
                [] -> list_to_binary(gettext:default_lang());
                [Lang|_] -> Lang
            end
    end.


get_atom_from_proplist(Key, Data) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> null;
        Value -> Value
    end.

get_atom_from_proplist(Key, Data, Default) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> Default;
        Value -> Value
    end.

get_binary_from_proplist(Key, Data) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> <<"">>;
        Value -> Value
    end.

get_binary_from_proplist(Key, Data, Default) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null  -> Default;
        Value -> Value
    end.

get_int_from_proplist(Key, Data) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> 0;
        Value -> get_int_value(Value)
    end.

get_int_from_proplist(Key, Data, Default) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> Default;
        Value -> get_int_value(Value)
    end.


get_int_value(Value) when is_binary(Value) ->
    binary_to_integer(Value);

get_int_value(Value) when is_list(Value) ->
    list_to_integer(Value);

get_int_value(Value) when is_integer(Value) ->
    Value.


is_expired(#paste{} = Paste) ->
    ExpireDatetime = iso8601:add_time(Paste#paste.created_at, 0, 0, Paste#paste.ttl),
    ExpireAt = calendar:datetime_to_gregorian_seconds(ExpireDatetime),
    Now = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())),
    Now >= ExpireAt.


new_id() ->
    list_to_binary(lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(?ID_CHARS)), ?ID_CHARS)]
                            ++ Acc
                   end, [], lists:seq(1, ?ID_LENGTH))).


paste_to_proplist(#paste{} = Record) ->
    Data = lists:zip(record_info(fields, paste), tl(tuple_to_list(Record))),
    Update = maybe_convert_iso8601_to_timetuple(created_at, Data),
    maybe_convert_iso8601_to_timetuple(updated_at, Update).


proplist_to_paste(Data) ->
    #paste{id = get_binary_from_proplist(id, Data, null),
           hostname = get_binary_from_proplist(hostname, Data),
           owner = get_binary_from_proplist(owner, Data),
           created_at = maybe_convert_timetuple_to_iso8601(created_at, Data),
           updated_at = maybe_convert_timetuple_to_iso8601(updated_at, Data),
           revision = get_int_from_proplist(revision, Data),
           ttl = get_int_from_proplist(ttl, Data),
           password = get_binary_from_proplist(password, Data),
           views = get_int_from_proplist(views, Data),
           syntax = get_binary_from_proplist(syntax, Data),
           line_numbers = get_atom_from_proplist(line_numbers, Data, false),
           content = get_binary_from_proplist(content, Data)}.


maybe_convert_iso8601_to_timetuple(Key, Data) ->
    case proplists:get_value(Key, Data, null) of
        null   -> Data;
        Value  ->
            New = proplists:delete(Key, Data),
            Formatted = iso8601:format(Value),
            lists:append(New, [{Key, Formatted}])
    end.


maybe_convert_timetuple_to_iso8601(Key, Data) ->
    case get_binary_from_proplist(Key, Data, null) of
        null      -> null;
        Date      -> iso8601:parse(Date)
    end.
