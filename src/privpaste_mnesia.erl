%% ------------------------------------------------------------------
%% Mnesia startup/cluster functionality
%% ------------------------------------------------------------------
-module(privpaste_mnesia).

%% API
-export([start/0,
         backup/0,
         reset/0,
         restore/0]).

-include("privpaste.hrl").

-define(TABLES, [pastes]).
-define(WAIT_TIMEOUT, 60000).

start() ->
    lager:log(info, self(), "Starting privpaste_mnesia"),
    mnesia:start(),
    maybe_initialize(),
    maybe_wait_for_tables().

backup() ->
    lager:log(info, self(), "Backing up the existing database"),
    mnesia:backup("/var/lib/privatepaste/backup.bup").

restore() ->
    lager:log(info, self(), "Restoring the database from a backup"),
    mnesia:restore("/var/lib/privatepaste/backup.bup", []).

create_schema(Nodes) ->
    lager:log(info, self(), "Creating schema with nodes: ~p", [Nodes]),
    case mnesia:create_schema(Nodes) of
        ok -> ok;
        {error, Reason} -> lager:error("Error creating schema: ~p", [Reason])
    end.

create_tables(Nodes) ->
    case mnesia:create_table(pastes,
                             [{attributes, record_info(fields, paste)},
                              {disc_copies, Nodes},
                              {record_name, paste},
                              {type, set}]) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            lager:log(error, self(), "Error creating table: ~p", [Reason]),
            error
    end.

has_tables() ->
    lists:all(fun(T) -> T == true end,
              [lists:member(T, mnesia:system_info(tables)) || T <- ?TABLES]).

is_node_reachable(Node) ->
    net_adm:ping(Node) =:= pong.

join(Nodes) ->
    BNode = lists:nth(1, Nodes),
    rpc:call(BNode, privpaste_mnesia, backup, []),
    All = maybe_add_local_node(Nodes),
    rpc:multicall(All, privpaste_mnesia, reset, []),
    ok = mnesia:create_schema(All),
    rpc:multicall(All, application, start, [mnesia]),
    ok = create_tables(All),
    rpc:call(BNode, privpaste_mnesia, restore, []).

maybe_initialize() ->
    case has_tables() of
        true -> ok;
        false ->
            Nodes = node_list(),
            case Nodes of
                [] ->
                    stopped = mnesia:stop(),
                    ok = mnesia:delete_schema([node()]),
                    create_schema([node()]),
                    mnesia:start(),
                    create_tables([node()]);
                _ ->
                    join(Nodes)
            end
    end.

maybe_wait_for_tables() ->
    lager:log(info, self(), "Waiting for tables: ~p", [?TABLES]),
    case mnesia:wait_for_tables(?TABLES, ?WAIT_TIMEOUT) of
        ok -> ok;
        {timeout, Tables} ->
            lager:log(error, self(), "Timeout waiting for tables: ~p", [Tables]),
            error;
        {error, Reason} ->
            lager:log(error, self(), "Error waiting for tables: ~p", [Reason]),
            error
    end.

maybe_add_local_node(Nodes) ->
    case lists:member(node(), Nodes) of
        true -> Nodes;
        false -> lists:merge([node()], Nodes)
    end.

maybe_remove_local_node(Nodes) ->
    case lists:member(node(), Nodes) of
        false -> Nodes;
        true -> lists:delete(node(), Nodes)
    end.

node_list() ->
    N1 = get_os_env_list(),
    Nodes = case N1 of
        undefined -> application:get_env(privatepaste, nodes, []);
        _  -> N1
    end,
    N2 = lists:takewhile(fun(N) -> is_node_reachable(N) end, Nodes),
    maybe_remove_local_node(N2).

get_os_env_list() ->
    case os:getenv("NODES") of
        false -> undefined;
        Value -> string:tokens(Value, ",")
    end.

reset() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).
