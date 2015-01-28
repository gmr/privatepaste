%% ------------------------------------------------------------------
%% Database Abstraction
%% ------------------------------------------------------------------
-module(privpaste_db).

-export([create_paste/2,
         delete_paste/1,
         get_paste/2,
         increment_paste_views/2,
         update_paste/3]).

-include("privpaste.hrl").

create_paste(Hostname, Paste) ->
    try
        ok = check_hostname(Hostname, Paste),
        ok = check_id_is_empty(Paste),
        ok = check_syntax(Paste),
        ok = check_ttl(Paste),
        NewPaste = Paste#paste{id=new_id()},
        Write = fun() ->
            mnesia:write(pastes, NewPaste, write)
        end,
        case mnesia:activity(transaction, Write) of
            ok  -> {ok, NewPaste};
            Err -> {error, Err}
        end
    catch
        error:{badmatch, hostname_mistmatch} -> conflict;
        error:{badmatch, id_is_set}          -> conflict;
        error:{badmatch, invalid_mode}       -> conflict;
        error:{badmatch, invalid_ttl}        -> conflict
    end.

delete_paste(Id) ->
    Delete = fun() ->
        mnesia:delete(pastes,  Id, write)
    end,
    mnesia:activity(transaction, Delete).

get_paste(Hostname, Id) ->
    Pattern = #paste{hostname=Hostname, id=Id, _='_'},
    Match = fun() ->
        case mnesia:match_object(pastes, Pattern, read) of
            [] -> not_found;
            Result ->
                Paste = hd(Result),
                try
                    ok = check_expiration(Paste),
                    {ok, Paste}
                catch
                    error:{badmatch, expired_paste} -> expired_paste(Id)
                end
        end
	end,
    {atomic, Response} = mnesia:transaction(Match),
    Response.

increment_paste_views(Hostname, Id) ->
    Pattern = #paste{hostname=Hostname, id=Id, _='_'},
    Update = fun() ->
        case mnesia:match_object(pastes, Pattern, write) of
            []       -> not_found;
            Result ->
                Paste = hd(Result),
                Incremented = Paste#paste{views=Paste#paste.views + 1},
                mnesia:write(pastes, Incremented, write)
        end
    end,
    mnesia:activity(transaction, Update).

update_paste(Hostname, Id, Paste) ->
    Pattern = #paste{hostname=Hostname, id=Id, _='_'},
    Update = fun() ->
        case mnesia:match_object(pastes, Pattern, write) of
            []       -> not_found;
            Result ->
                OldPaste = hd(Result),
                try
                    ok = check_ids(Id, Paste),
                    ok = check_expiration(OldPaste),
                    ok = check_expiration(Paste),
                    ok = check_hostname(Hostname, OldPaste),
                    ok = check_hostname(Hostname, Paste),
                    ok = check_syntax(Paste),
                    ok = check_ttl(Paste),
                    Updated = Paste#paste{id=Id,
                                          updated_at=calendar:universal_time(),
                                          revision=Paste#paste.revision + 1},
                    {mnesia:write(pastes, Updated, write), Updated}
                catch
                    error:{badmatch, id_mistmatch}       -> conflict;
                    error:{badmatch, expired_paste}      -> expired_paste(Id);
                    error:{badmatch, hostname_mistmatch} -> conflict;
                    error:{badmatch, invalid_mode}       -> conflict;
                    error:{badmatch, invalid_ttl}        -> conflict
                end
        end
    end,
    mnesia:activity(transaction, Update).

%% ------------------------------------------------------------------
%% Internal Methods
%% ------------------------------------------------------------------

expired_paste(Id) ->
    lager:log(debug, self(), "Removing expired paste ~s", [Id]),
    delete_paste(Id),
    not_found.

new_id() ->
    Base = uuid:uuid_to_string(uuid:get_v4()),
    list_to_binary(string:substr(Base, 1, 8)).

%% ------------------------------------------------------------------
%% Validation Methods
%% ------------------------------------------------------------------

check_expiration(#paste{} = Paste) ->
    ExpireDatetime = iso8601:add_time(Paste#paste.created_at, 0, 0, Paste#paste.ttl),
    ExpireAt = calendar:datetime_to_gregorian_seconds(ExpireDatetime),
    Now = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())),
    case Now >= ExpireAt of
        true  -> expired_paste;
        false -> ok
    end.

check_hostname(Expectation, #paste{}=Paste) ->
    case Expectation == Paste#paste.hostname of
        true  -> ok;
        false -> hostname_mismatch
    end.

check_id_is_empty(#paste{} = Paste) ->
    case Paste#paste.id == null of
        true  -> ok;
        false -> id_not_empty
    end.

check_ids(Expectation, #paste{}=Paste) ->
    case Expectation == Paste#paste.id of
        true  -> ok;
        false -> id_mismatch
    end.

check_syntax(#paste{} = Paste) ->
    Modes = proplists:get_keys(?MODES),
    case lists:member(Paste#paste.syntax, Modes) of
        true  -> ok;
        false -> invalid_mode
    end.

check_ttl(#paste{} = Paste) ->
    case Paste#paste.ttl > ?TTL_MAX of
        true  -> invalid_ttl;
        false -> ok
    end.
