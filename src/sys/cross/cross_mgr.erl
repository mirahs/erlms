%% -*- coding: latin-1 -*-
-module(cross_mgr).

-behaviour(gen_server).

-export([
    start_link/0

    ,cross_add/1
    ,cross_del/1

    ,cast/4
    ,cast/5
    ,call/5
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
-include("cross.hrl").

-record(state, {}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


cross_add(Host) ->
    gen_server:call(?MODULE, {cross_add, Host}).

cross_del(Id) ->
    gen_server:call(?MODULE, {cross_del, Id}).


%% 异步调用所有远端服务器
cast(all, M, F, A) ->
    gen_server:cast(?MODULE, {cast, all, M, F, A}).

%% 异步调用指定 id 的远端服务器
cast(node, Id, M, F, A) ->
    case cross_srv_online:get_srv(Id) of
        #cross_srv{node = Node, mpid = Mpid} when is_pid(Mpid) ->
            rpc:cast(Node, M, F, A),
            {ok};
        _ ->
            ?ERR("中央服还没有连接上[~w],无法异步调用[~w,~w,~p]", [Id, M, F, A]),
            {false, not_connected}
    end.

%% 同步调用指定 id 的远端服务器
call(node, Id, M, F, A) ->
    case cross_srv_online:get_srv(Id) of
        #cross_srv{node = Node, mpid = Mpid} when is_pid(Mpid) ->
            rpc:call(Node, M, F, A);
        _ ->
            ?ERR("中央服还没有连接上[~w],无法同步调用[~w,~w,~p]", [Id, M, F, A]),
            {false, not_connected}
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    cross_srv_online:init(),
    init_srv(),
    {ok, #state{}}.

handle_call({cross_add, Host}, _From, State) ->
    do_start_srv(Host),
    {reply, ok, State};

handle_call({cross_del, Id}, _From, State) ->
    do_del_srv(Id),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({cast, all, M, F, A}, State) ->
    cross_srv_online:cast(M, F, A),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 初始化 cross_srv
init_srv() ->
    {ok, Hosts} = tbl_host:all(),
    Fun = fun(Host) -> do_start_srv(Host) end,
    lists:foreach(Fun, Hosts).

%% 启动 cross_srv
do_start_srv(Host = #{name := Name}) ->
    Cs = to_srv(Host),
    case cross_srv:start_link(Cs) of
        {ok, _Pid} -> ignore;
        _Err -> ?ERR("初始化镜像服务器[Name=~s]出错:~p", [Name, _Err])
    end.

%% 删除 cross_srv
do_del_srv(Id) ->
    case cross_srv_online:get_srv(Id) of
        #cross_srv{pid = Pid} ->
            cross_srv:stop(Pid),
            cross_srv_online:delete(Id);
        false -> skip
    end.


%% 转换成 cross_srv
to_srv(#{id := Id, name := Name}) ->
    Node = util:to_atom("erlms_client@" ++ util:to_list(Name)),
    #cross_srv{id = Id, node = Node}.
