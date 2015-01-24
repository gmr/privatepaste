%% ------------------------------------------------------------------
%% Database Module
%% ------------------------------------------------------------------
-module(privatepaste_db).

-behaviour(gen_server).

-export([start_link/0,
init/1,
handle_call/3,
handle_cast/2,
handle_info/2,
terminate/2,
code_change/3]).

-include("privatepaste.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    CSR = mnesia:create_schema([node()]),
    SR = mnesia:start(),
    CTR = mnesia:create_table(pastes, [{attributes, record_info(fields, paste)},
                                       {disc_copies, [node()]},
                                       {record_name, paste},
                                       {type, bag}]),
    lager:log(info, self(), "mnesia started: ~p ~p ~p", [CSR, SR, CTR]),
    {ok, {}}.

handle_call(Request, _From, State) ->
    case Request of
        {insert_paste, Record} -> {reply, insert_paste(Record), State};
        stats                  -> {reply, mnesia_stats(), State};
        else                   -> {noreply, State}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    mnesia:stop(),
    lager:log(info, self(), "mnesia stopped"),
    {ok}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

mnesia_stats() ->
    [{active_transactions, length(mnesia:system_info(transactions))},
     {queued_transactions, length(mnesia:system_info(lock_queue))},
     {transaction_commits, mnesia:system_info(transaction_commits)},
     {transaction_failures, mnesia:system_info(transaction_failures)}].

insert_paste(Record) ->
    lager:log(info, self(), "Writing paste to mnesia: ~p~n", [Record]),
    F = fun() ->
        mnesia:write(pastes, Record, write)
    end,
    mnesia:activity(transaction,F).
