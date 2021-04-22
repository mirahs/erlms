%%% -*- coding: latin-1 -*-
-module(stat_api).

-export([
    minute5/0
]).

-export([
    report_mem/5
    ,report_loadavg/4
    ,report_cpu_used/4
]).

-include("common.hrl").
-include("cross.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% 每5分钟
minute5() ->
    Time    = util:unixtime(),
    HiKey   = util:time_sep_hi5(),
    ?INFO("HiKey:~s", [HiKey]),
    stat_loadavg(Time, HiKey),
    stat_mem(Time, HiKey),
    stat_cpu_used(Time, HiKey), %cpu使用率统计要暂停1秒, 所以放到最后
    ok.


%%%===================================================================
%%% 数据上报
%%%===================================================================

report_mem(Hostname, Time, HiKey, MemTotal, MemUsed) ->
    case tbl_host:name_to_id(Hostname) of
        {ok, HostId} ->
            tbl_stat_mem:add([{host_id, HostId}, {time, Time}, {hi_key, HiKey}, {mem_total, MemTotal}, {mem_used, MemUsed}]);
        _ -> ?ERR("主机[~s]不存在")
    end.

report_loadavg(Hostname, Time, HiKey, {Load1, Load5, Load15}) ->
    case tbl_host:name_to_id(Hostname) of
        {ok, HostId} ->
            tbl_stat_load:add([{host_id, HostId}, {time, Time}, {hi_key, HiKey}, {load1, Load1}, {load5, Load5}, {load15, Load15}]);
        _ -> ?ERR("主机[~s]不存在")
    end.

report_cpu_used(Hostname, Time, HiKey, CpuUsed) ->
    case tbl_host:name_to_id(Hostname) of
        {ok, HostId} ->
            tbl_stat_cpu_used:add([{host_id, HostId}, {time, Time}, {hi_key, HiKey}, {used, CpuUsed}]);
        _ -> ?ERR("主机[~s]不存在")
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

stat_loadavg(Time, HiKey) ->
    case util_os:loadavg() of
        {ok, Loadavg} ->
            cross:cast(?cross_type_server, ?MODULE, report_loadavg, [util:hostname(), Time, HiKey, Loadavg]);
        {false, Reason} ->
            ?ERR("Reason:~p", [Reason])
    end.

stat_cpu_used(Time, HiKey) ->
    case util_os:cpu_used() of
        {ok, CpuUsed} ->
            cross:cast(?cross_type_server, ?MODULE, report_cpu_used, [util:hostname(), Time, HiKey, CpuUsed]);
        {false, Reason} ->
            ?ERR("Reason:~p", [Reason])
    end.

stat_mem(Time, HiKey) ->
    case util_os:meminfo() of
        {ok, Info} ->
            Total= util:to_integer(proplists:get_value("MemTotal", Info)),
            Used = Total -  util:to_integer(proplists:get_value("MemFree", Info)) - util:to_integer(proplists:get_value("Buffers", Info)) - util:to_integer(proplists:get_value("Cached", Info)),
            cross:cast(?cross_type_server, ?MODULE, report_mem, [util:hostname(), Time, HiKey, Total, Used]);
        {false, Reason} ->
            ?ERR("Reason:~p", [Reason])
    end.
