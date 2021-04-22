%% -*- coding: latin-1 -*-
-module(cross_srv).

-behaviour(gen_server).

-export([
    start_link/1

    ,stop/1
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

-define(re_conn_time, 10000). % 重新连接的时间(毫秒)


%%%===================================================================
%%% API
%%%===================================================================

start_link(CrossSrv) ->
    gen_server:start_link(?MODULE, [CrossSrv], []).


%% 关闭cross_srv
stop(Pid) ->
    Pid ! stop.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Cs = #cross_srv{node = Node}]) ->
    #{cookie := Cookie} = app_conf:node(Node, ?salt_conf),
    erlang:set_cookie(Node, Cookie),
    TimerRef = erlang:send_after(100, self(), {check_online, 0}),
    NewCs = Cs#cross_srv{pid = self(), timer_ref = TimerRef},
    cross_srv_online:save(NewCs),
    {ok, NewCs}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% 定时检查远端服务器是否上线
handle_info({check_online, N}, State = #cross_srv{node = Node, mpid = undefined}) ->
    case N > 0 of
        true -> ?INFO("~w 连接不上，尝试了 ~w 次，正在重连", [Node, N]);
        false -> ignore
    end,
    rpc:cast(Node, cross, ready, [cross:get_node_type(), node(), self()]),
    TimerRef = erlang:send_after(?re_conn_time, self(), {check_online, N+1}),
    NewState = State#cross_srv{timer_ref = TimerRef},
    cross_srv_online:save(NewState),
    {noreply, NewState};

%% 远端服务器已连接
handle_info({ready, Pid}, State = #cross_srv{node = Node, timer_ref = TimerRef}) ->
    ?INFO("已经与服务器[~w]建立连接", [Node]),
    Mref = erlang:monitor(process, Pid),
    erlang:cancel_timer(TimerRef),
    NewState = State#cross_srv{mpid = Pid, mref = Mref, timer_ref = undefined},
    cross_srv_online:save(NewState),
    {noreply, NewState};

%% 远端服务器已断开连接
handle_info({'DOWN', Mref, _Type, _Object, _Reason} = R, State = #cross_srv{node = Node, mref = Mref}) ->
    ?INFO("与服务器[~w]的连接已经断开:~n~w", [Node, R]),
    TimerRef = erlang:send_after(?re_conn_time, self(), {check_online, 0}),
    NewState = State#cross_srv{mpid = undefined, mref = undefined, timer_ref = TimerRef},
    cross_srv_online:save(NewState),
    erlang:demonitor(Mref),
    {noreply, NewState};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info(Info, State = #cross_srv{node = Node}) ->
    ?ERR("~w 的镜像进程收到未知消息:~w", [Node, Info]),
    {noreply, State}.

terminate(Reason, #cross_srv{id = Id, node = Node}) ->
    case Reason of
        normal -> ?INFO("连接到[~w]的镜像进程已经正常退出", [Node]);
        _ -> ?ERR("连接到[~w]的镜像进程异常退出:~w", [Node, Reason])
    end,
    cross_srv_online:delete(Id),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
