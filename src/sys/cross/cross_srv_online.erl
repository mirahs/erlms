%% -*- coding: latin-1 -*-
-module(cross_srv_online).

-export([
    init/0
    ,get_srvs/0
    ,get_srv/1
    ,get_ids/0
    ,save/1
    ,delete/1
    ,check/1
    ,cast/3
]).

-include("common.hrl").
-include("cross.hrl").


%%%===================================================================
%%% API
%%%===================================================================

init() ->
    ets:new(?ets_cross_srv_online, [set, named_table, public, {keypos, #cross_srv.id}]).

%% 获取所有 cross_srv
get_srvs() ->
    ets:tab2list(?ets_cross_srv_online).

%% 获取指定的 cross_srv
get_srv(Id) ->
    case ets:lookup(?ets_cross_srv_online, Id) of
        [Srv = #cross_srv{}] -> Srv;
        _ -> false
    end.

%% 获取所有 ID
get_ids() ->
    [Id || #cross_srv{id = Id} <- get_srvs()].

%% 保存在线的cross_srv
save(Cs) ->
    ets:insert(?ets_cross_srv_online, Cs).

%% 删除 cross_srv
delete(Id) when is_integer(Id) ->
    ets:delete(?ets_cross_srv_online, Id);
delete(Pid) when is_pid(Pid) ->
    ets:match_delete(?ets_cross_srv_online, #cross_srv{pid = Pid, _ = '_'}).

%% 检查节点是否连接上
check(Id) ->
    case get_srv(Id) of
        #cross_srv{mpid = MPid} when is_pid(MPid) -> true;
        _ -> false
    end.

cast(M, F, A) ->
    L = get_srvs(),
    do_cast(L, M, F, A).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 广播
do_cast([], _M, _F, _A) -> ok;
do_cast([Id | T], M, F, A) when is_integer(Id)->
    case get_srv(Id) of
        #cross_srv{node = Node, mpid = Mpid} when is_pid(Mpid) ->
            rpc:cast(Node, M, F, A);
        _ ->
            ?ERR("中央服还没有连接上[~w]，无法异步调用[~w,~w,~w]", [Id, M, F, A])
    end,
    do_cast(T, M, F, A).
