%% ------------------------------------------------------------------
%% Paste Endpoint
%% ------------------------------------------------------------------
-module(privatepaste_paste).

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         create_paste/2,
         terminate/3]).

-include("privatepaste.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, create_paste}], Req, State}.

content_types_provided(Req, State) ->
    {[{?MIME_TYPE_JSON, handle_json},
      {?MIME_TYPE_HTML, handle_html}], Req, State}.

create_paste(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Data} = jiffy:decode(Body),
    Response = gen_server:call(privatepaste_db,
                               {insert_paste,
                                #paste{id = new_paste_id(),
                                       hostname = get_binary(hostname, Data),
                                       owner = get_binary(owner, Data),
                                       created_at = get_binary(created_at, Data),
                                       updated_at = get_binary(updated_at, Data),
                                       revision = get_int(revision, Data),
                                       ttl = get_int(ttl, Data),
                                       password = get_binary(password, Data),
                                       views = get_int(views, Data),
                                       syntax = get_binary(syntax, Data),
                                       line_numbers = get_atom(line_numbers, Data, false),
                                       code_folding = get_atom(code_folding, Data, false),
                                       content = get_binary(content, Data)}}),
    lager:log(info, self(), "~p~n", [Response]),
    {true, Req2, State}.


get_atom(Key, Data, Default) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> Default;
        Value -> Value
    end.

get_binary(Key, Data) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> <<"">>;
        Value -> Value
    end.

get_int(Key, Data) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> 0;
        Value -> Value
    end.


terminate(_Reason, _Req, _State) ->
    ok.



new_paste_id() ->
    Initial = random:uniform(62) - 1,
new_paste_id(<<Initial>>, 7).
new_paste_id(Bin, 0) ->
    Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
    << <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
    new_paste_id(Bin, Rem) ->
        Next = random:uniform(62) - 1,
        new_paste_id(<<Bin/binary, Next>>, Rem - 1).


handle_html(Req, State) ->
    {jiffy:encode({<<"response">>, true}), Req, State}.


handle_json(Req, State) ->
    {jiffy:encode({<<"response">>, true}), Req, State}.
