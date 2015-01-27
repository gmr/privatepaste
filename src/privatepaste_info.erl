-module(privatepaste_info).

-export([init/2,
         content_types_provided/2,
         handle_html/2,
         handle_json/2,
         terminate/3]).

-include("privatepaste.hrl").

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{?MIME_TYPE_HTML, handle_html},
		{?MIME_TYPE_JSON, handle_json}
	], Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_html(Req, State) ->
    Opts = [{translation_fun, ?TRANSLATE,
            {locale, privatepaste_util:get_language(Req)}],
    {ok, Body} = info_dtl:render(data(Req, false), Opts),
    {Body, Req, State}.

handle_json(Req, State) ->
    {jsx:encode(data(Req, true)), Req, State}.

data(Req, JSON) ->
    [{headers, {maybe_escape_keys(cowboy_req:headers(Req), JSON)}},
     {apps, [[list_to_binary(atom_to_list(A)),
              list_to_binary(D),
              list_to_binary(V)] || {A, D, V} <- application:loaded_applications()]},
     {memory, {[{list_to_binary(atom_to_list(K)), V} || {K, V} <- erlang:memory()]}},
     {language, privatepaste_util:get_language(Req)},
     {languages, [list_to_binary(L) || L <- gettext:all_lcs()]},
     {node, erlang:node()},
     {system_version, list_to_binary(erlang:system_info(system_version))},
     {proc_count, erlang:system_info(process_count)},
     {architecture, list_to_binary(erlang:system_info(system_architecture))},
     {mnesia, {[{list_to_binary(atom_to_list(K)), V} || {K, V} <- mnesia_stats()]}}].

maybe_escape_keys(Headers, JSON) when JSON =:= true ->
    [{re:replace(K, "-", "_", [{return,binary}]), V} || {K, V} <- Headers];

maybe_escape_keys(Headers, _) ->
    Headers.

mnesia_stats() ->
    [{active_transactions, length(mnesia:system_info(transactions))},
    {queued_transactions, length(mnesia:system_info(lock_queue))},
    {transaction_commits, mnesia:system_info(transaction_commits)},
    {transaction_failures, mnesia:system_info(transaction_failures)}].
