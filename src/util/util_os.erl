%%% -*- coding: latin-1 -*-
-module(util_os).

-export([
    loadavg/0
    ,cpu_used/0
    ,meminfo/0
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

loadavg() ->
    case file:open("/proc/loadavg", [read]) of
        {ok, Fd} ->
            Line = io:get_line(Fd, ''),
            file:close(Fd),
            [Load1, Load5, Load15 | _] = string:split(Line, " ", all),
            {ok, {Load1, Load5, Load15}};
        {error, Reason} -> {false, Reason}
    end.

%% cpu 使用率
%% 参考文档
%%     https://blog.csdn.net/ibless/article/details/85177175
%%     https://www.jianshu.com/p/541d8efcbb78
%%     https://blog.csdn.net/gaogaoshan/article/details/38038743
cpu_used() ->
    case file:open("/proc/stat", [read]) of
        {ok, Fd} ->
            Line = io:get_line(Fd, ''),
            file:close(Fd),
            Stat = #{idle := Idle} = process_stat(Line),
            Total= lists:sum(maps:values(Stat)),

            util_proc:sleep(1000),

            {ok, Fd2} = file:open("/proc/stat", [read]),
            Line2= io:get_line(Fd2, ''),
            file:close(Fd2),
            Stat2 = #{idle := Idle2} = process_stat(Line2),
            Total2= lists:sum(maps:values(Stat2)),

            DiffIdle = Idle2 - Idle,
            DiffTotal= Total2 - Total,
            DiffUsed = DiffTotal - DiffIdle,

            {ok, util_math:float((DiffUsed / DiffTotal) * 100, 2)};
        {error, Reason} -> {false, Reason}
    end.

meminfo() ->
    case file:open("/proc/meminfo", [read]) of
        {ok, Fd} -> process_meminfo(Fd);
        {error, Reason} -> {false, Reason}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

process_stat(Line) ->
    [_Cpu, User, Nice, System, Idle, Iowait, Irq, Softirq, Steal, Guest, GuestNice] = [Item || Item <- string:split(string:trim(Line), " ", all), Item =/= ""],
    #{
        user => util:to_integer(User), nice => util:to_integer(Nice), system => util:to_integer(System), idle => util:to_integer(Idle)
        ,iowait => util:to_integer(Iowait), irq => util:to_integer(Irq), softirq => util:to_integer(Softirq), steal => util:to_integer(Steal)
        ,guest => util:to_integer(Guest), guest_nice => util:to_integer(GuestNice)
    }.

process_meminfo(Fd) ->
    process_meminfo(Fd, []).
process_meminfo(Fd, Info) ->
    case io:get_line(Fd, '') of
        eof -> {ok, lists:reverse(Info)};
        Line ->
            [Name, Var] = string:split(Line, ":"),
            Var2 = string:trim(Var),
            [Var3 | _] = string:split(Var2, " "),
            process_meminfo(Fd, [{Name, Var3} | Info])
    end.
