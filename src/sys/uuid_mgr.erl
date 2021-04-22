%%% -*- coding: latin-1 -*-
-module(uuid_mgr).

-behaviour(gen_server).

-export([
    start_link/0

    ,get/1
    ,get/2
]).

-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-include("common.hrl").

-record(state, {tab_id}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get(Key) ->
    get(Key, 1).
get(Key, DV) ->
    gen_server:call(?MODULE, {get, Key, DV}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    TabId = ets:new(?MODULE, [set, public, {keypos, 1}]),
    {ok, KVs0} = tbl_uuid:all(),
    KVs = [{util:to_atom(Key), Val} || #{key := Key, val := Val} <- KVs0],
    ets:insert(TabId, KVs),
    {ok, #state{tab_id = TabId}}.

handle_call({get, Key, DV}, _From, State = #state{tab_id = TabId}) ->
    Reply =
        case ets:lookup(TabId, Key) of
            [{Key, Val}] -> Val;
            _ ->
                tbl_uuid:add([{key, Key}, {val, DV}]),
                DV
        end,
    ValN = Reply + 1,
    {ok, _AffectedRows} = tbl_uuid:update(Key, [{val, ValN}]),
    ets:insert(TabId, {Key, ValN}),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
