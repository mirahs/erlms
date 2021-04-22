%% -*- coding: latin-1 -*-
-module(cross).

-behaviour(gen_server).

-export([
    start_link/0

    ,get_node_type/0
    ,ready/3
    ,call/4
    ,cast/4
    ,is_connect/1
    ,is_cross/0
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

-record(state, {
    nodes = []  % 中央服节点列表
}).

-record(cross_node, {
    type        % 中央服节点类型
    ,node       % 中央服节点名称
    ,mpid       % 进程pid
    ,mref       % 进程监控ref
}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% 获取节点类型
%% Type: atom, 跨服结点类型, 具体看 cross.hrl 的 cross_type_xx
get_node_type() ->
    sys_env:get(node_type).

%% 中央服节点启动后通知当前节点
ready(Type, Node, Mpid) ->
    gen_server:cast(?MODULE, {ready, Type, Node, Mpid}).

%% @spec call(Type, M, F, A) ->
%% Type = atom()
%% M = atom()
%% F = atom()
%% A = term
%% 对中央服发起同步调用，
%% Type: 中央服结点类型, 具体看 cross.hrl 的 cross_type_xx
call(Type, M, F, A) when is_atom(Type) ->
    case get_cross_node(Type) of
        #cross_node{node = Node} -> rpc:call(Node, M, F, A);
        _ -> {error, cross_not_ready}
    end.

%% cast(Type, M, F, A) ->
%% Type = atom()
%% M = atom()
%% F = atom()
%% A = term()
%% 对中央服发起异步调用
%% Type: 中央服结点类型, 具体看 cross.hrl 的 cross_type_xx
cast(Type, M, F, A) when is_atom(Type) ->
    case get_cross_node(Type) of
        #cross_node{node = Node} -> rpc:cast(Node, M, F, A);
        _ -> {error, center_not_ready}
    end.

%% @spec is_connect(Type) -> {true, {Node, Pid}} | false
%% Type =atom()
%% Node = atom()
%% Pid = pid()
%% 判断是否连接了Type中央结点
%% Type: 中央服结点类型, 具体看 cross.hrl 的 cross_type_xx
is_connect(Type) ->
    case get_cross_node(Type) of
        #cross_node{node = Node, mpid = Pid} when is_pid(Pid) -> {true, {Node, Pid}};
        _ -> false
    end.

%% @spec is_cross() -> false | true
%% 判断本节点是否中央服
is_cross() ->
    case get_node_type() of
        ?cross_type_server -> true;
        _ -> false
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),

    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({ready, Type, Node, Pid}, State = #state{nodes = Nodes})  ->
    case get_cross_node(Type) of
        undefined ->
            ?INFO("已经与中央服[type=~w,node=~w]建立连接", [Type, Node]),
            Mref = erlang:monitor(process, Pid),
            NewNodes = put_cross_node(Nodes, Type, Node, Pid, Mref),
            erlang:send(Pid, {ready, self()}),
            {noreply, State#state{nodes = NewNodes}};
        #cross_node{node = Node, mpid = Pid} ->
            ?INFO("已经与中央服[type=~w,node=~w]建立连接, 重复请求", [Type, Node]),
            erlang:send(Pid, {ready, self()}),
            {noreply, State};
        #cross_node{node = OldNode, mref = OldRef} ->
            ?INFO("已经与中央服[type=~w,node=~w]建立连接, 新的请求链接[node=~w]", [Type, OldNode, Node]),
            erlang:demonitor(OldRef),
            Mref = erlang:monitor(process, Pid),
            NewNodes = put_cross_node(Nodes, Type, Node, Pid, Mref),
            erlang:send(Pid, {ready, self()}),
            {noreply, State#state{nodes = NewNodes}}
    end;

handle_cast(Request, State) ->
    ?ERR("收到未知消息:~w", [Request]),
    {noreply, State}.

handle_info({'DOWN', Mref, _Type, _Object, Reason}, State = #state{nodes = Nodes}) ->
    case lists:keyfind(Mref, #cross_node.mref, Nodes) of
        #cross_node{type = NodeType, node = Node} ->
            ?INFO("与中央服[type=~w,node=~w]的连接已经断开:~n~w", [NodeType, Node, Reason]),
            NewNodes = delete_node(NodeType, Nodes),
            erlang:demonitor(Mref),
            {noreply, State#state{nodes = NewNodes}};
        _ ->
            ?ERR("收到中央结点断开的消息, 但不存在此中央结点[mref=~w]", [Mref]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    ?ERR("收到未知消息:~w", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 获取跨服中央的 node
get_cross_node(Type) ->
    case sys_env:get(?env_cross) of
        Cross when is_list(Cross) ->
            case lists:keyfind(Type, #cross_node.type, Cross) of
                Node = #cross_node{} -> Node;
                _ -> undefined
            end;
        _ -> undefined
    end.

%% 设置新的 node
put_cross_node(Nodes, Type, Node, Pid, Mref) ->
    CrossNode = #cross_node{type = Type, node = Node, mpid = Pid, mref = Mref},
    NewNodes = [CrossNode | delete_node(Type, Nodes)],
    sys_env:set(?env_cross, NewNodes),
    NewNodes.

%% 删除结点
delete_node(Type, Nodes) ->
    NewNodes = lists:keydelete(Type, #cross_node.type, Nodes),
    sys_env:set(?env_cross, NewNodes),
    NewNodes.
