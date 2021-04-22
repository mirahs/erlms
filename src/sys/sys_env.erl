%% -*- coding: latin-1 -*-
-module(sys_env).

-behaviour(gen_server).

-export([
    start_link/1
    ,get/1
    ,get/2
    ,set/2
    ,save/2
    ,del/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-include("common.hrl").
-include("cross.hrl").


%% @spec get(Key) -> term()
%% Key = atom()
%% @doc 取出相应设置
get(Key) when is_atom(Key) ->
    case ets:lookup(sys_env, Key) of
        [{_Key, Val}] -> Val;
        _ ->
            undefined
    end.

get(Key, Def) when is_atom(Key) ->
    case ets:lookup(sys_env, Key) of
        [{_Key, Val}] -> Val;
        _ -> Def
    end.

%% @spec set(Key, Val) -> ok
%% Key = atom()
%% Val = term()
%% @doc 动态修改设置
%% 注意:进行并发操作get/set同一key时的事务性需要使用者来保证
set(Key, Val) when is_atom(Key) ->
    ets:insert(sys_env, {Key, Val}).

%% @spec save(Key, Val) -> ok | {error, Err}
%% Key = atom()
%% Val = term()
%% Err = term()
%% @doc 设置并保存指定key的值到数据库
%% 注意:当数据库异常时该函数执行时间会比较长
%% 注意:不可以保存pid()数据
save(Key, Val) when is_atom(Key) ->
    set(Key, Val),
    dets:insert(sys_env, {Key, Val}).

%% @spec del(Key) -> ok | {error, Err}
%% Key = atom()
%% Err = term()
%% @doc 删除一个键值对
del(Key) when is_atom(Key) ->
    ets:delete(sys_env, Key),
    dets:delete(sys_env, Key).

%% @doc 启动环境变量服务器
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


init([SupName, NodeType, DirVar]) ->
    process_flag(trap_exit, true),
    ets:new(sys_env, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]),
    dets:open_file(sys_env, [{file, DirVar ++ "sys_env.dets"}, {keypos, 1}, {type, set}]),
    dets:to_ets(sys_env, sys_env),
    set(sup_name, SupName),
    set(node_type, NodeType),
    set(dir_var, DirVar),
    State = #state{},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(sys_env),
    ?INFO("关闭sys_env"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
