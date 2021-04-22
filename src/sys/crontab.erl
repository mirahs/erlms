%% -*- coding: latin-1 -*-
-module(crontab).

-behaviour(gen_server).

-export([
    start_link/1
]).

-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-include("cross.hrl").
-include("cron.hrl").

-record(state, {type}).


%%%===================================================================
%%% 配置
%%%===================================================================

%% 	分  					时 						日					月				周			MFA
%% {[0-59]					[0-23]					[1-31]				1-12]			[1-7],		{M,F,A}}
rule(?cross_type_client) ->
    [
        {[0,5,10,15,20,25,30,35,40,45,50,55], all, all, all, all, {stat_api, minute5, []}} % 统计-每5分钟
    ];
rule(_Type) ->
    [].


%%%===================================================================
%%% API
%%%===================================================================

start_link(Type) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Type], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Type]) ->
    ticket(),
    {ok, #state{type = Type}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_timer, State = #state{type = Type}) ->
    ticket(),
    task_run(Type),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% timer 定时
ticket() ->
    {_, _, Sec} = util:time(),
    Timer = erlang:send_after(timer:seconds(60 - Sec + 1), self(), check_timer),
    erlang:put(crontab_timer, Timer).

task_run(Type) ->
    LocalTime = {Date, _Time} = util:localtime(),
    Week = calendar:day_of_the_week(Date),
    task_run(rule(Type), {LocalTime, Week}),

    task_run_web(cron_util:cron_get(), {LocalTime, Week}).

task_run([Task = {_Min, _Hour, _Day, _Month, _Week, {M, F, A}} | Tasks], Arg = {LocalTime,Week}) ->
    case check_time(Task, LocalTime, Week) of
        true ->
            erlang:spawn(M, F, A),
            task_run(Tasks, Arg);
        false -> task_run(Tasks, Arg)
    end;
task_run([], _Arg) ->
    ok.

task_run_web([Cron = #cron{expr = #cron_expr{minute = _Min, hour = _Hour, day = _Day, month = _Month, week = _Week}} | Tasks], Arg = {LocalTime, Week}) ->
    Task = {_Min, _Hour, _Day, _Month, _Week, undefined},
    case check_time(Task, LocalTime, Week) of
        true ->
            erlang:spawn(cron, run, [Cron]),
            task_run_web(Tasks, Arg);
        false -> task_run_web(Tasks, Arg)
    end;
task_run_web([], _Arg) ->
    ok.


%% 时间检查
check_time({Min, Hour, Day, Month, WeekD, _MFA}, {{_Y,M,D}, {H,I,_S}}, Week) ->
    check_time([{Min, I}, {Hour, H}, {Day, D}, {Month, M}, {WeekD, Week}]).

check_time([]) -> true;
check_time([H | T]) ->
    case check_time2(H) of
        true -> check_time(T);
        false -> false
    end.

check_time2({all, _NowTime}) -> true;
check_time2({[], _NowTime}) -> true;
check_time2({TaskTime, NowTime}) when is_integer(TaskTime) -> TaskTime =:= NowTime;
check_time2({TaskTime, NowTime}) -> lists:member(NowTime, TaskTime).
