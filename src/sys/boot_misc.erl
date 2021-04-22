%% -*- coding: latin-1 -*-
-module(boot_misc).

-export([
    swap_sup_child/1
    ,start_sup_child/3
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% 处理监控树列表
swap_sup_child(L) ->
    lists:map(fun
                  ({Id, {M, F, A}, Restart, Shutdown, Type, Mod}) ->  {Id, {boot_misc, start_sup_child, [M, F, A]}, Restart, Shutdown, Type, Mod};
                  ({Id, {M, F, A}}) ->  {Id, {boot_misc, start_sup_child, [M, F, A]}, transient, 100000, worker, [Id]}
              end, L).

%% 启动监控树子进程
start_sup_child(M, F, A) ->
    ?INFO("正在启动[~w]", [M]),
    case erlang:apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            ?INFO("完成启动[~w]", [M]),
            {ok, Pid};
        Err ->
            ?INFO("启动出错[~w]: ~n~p", [M, Err]),
            Err
    end.
