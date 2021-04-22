%% -*- coding: latin-1 -*-
-module(util_proc).

-export([
    is_alive/1
    ,sleep/1
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% @spec is_process_alive(P) -> true | false
%% P = pid()
%% @doc 检查进程是否存活(可检查远程节点上的进程)
is_alive(P) when is_pid(P) ->
    case node() =:= node(P) of
        true ->
            erlang:is_process_alive(P);
        false ->
            case rpc:call(node(P), erlang, is_process_alive, [P]) of
                true -> true;
                false -> false;
                _ -> false
            end
    end;
is_alive(_Pid) ->
    false.

%% @spec sleep(T) -> ok
%% T = integer()
%% @doc 程序暂停执行时长(单位:毫秒)
sleep(T) ->
    receive
    after T ->
        true
    end.
