%% -*- coding: latin-1 -*-
-module(cron).

-export([
    client_sync/3
    ,client_sync/5
    ,client_sync/6
    ,client_sync_async/5
    ,client_sync_cb/1

    ,run/1
    ,run_cb/5
]).

-include("common.hrl").
-include("cross.hrl").
-include("web_adm.hrl").
-include("cron.hrl").


%%%===================================================================
%%% API
%%%===================================================================

client_sync(Id, CronId, LogDetailId) ->
    client_sync(Id, CronId, undefined, undefined, LogDetailId, true).
client_sync(Id, CronId, ExprErlang, Cmd, LogDetailId) ->
    client_sync(Id, CronId, ExprErlang, Cmd, LogDetailId, false).
client_sync(Id, CronId, ExprErlang, Cmd, LogDetailId, IsDel) ->
    cross_mgr:cast(node, Id, ?MODULE, client_sync_async, [CronId, ExprErlang, Cmd, LogDetailId, IsDel]).

client_sync_async(CronId, ExprErlang, Cmd, LogDetailId, IsDel) ->
    ?IF(IsDel, cron_util:cron_del(CronId), cron_util:cron_add(CronId, ExprErlang, Cmd)),
    cross:cast(?cross_type_server, ?MODULE, client_sync_cb, [LogDetailId]).

client_sync_cb(LogDetailId) ->
    {ok, #{log_id := LogId}} = tbl_log_cron_sync_detail:get(LogDetailId),
    tbl_log_cron_sync:cnt_succ_add(LogId),
    tbl_log_cron_sync_detail:update(LogDetailId, [{state, ?adm_cron_sync_state_succ}, {time_succ, util:unixtime()}]),
    ok.


run(#cron{id = CronId, cmd = Cmd}) ->
    statistics(runtime),
    statistics(wall_clock),

    Result = os:cmd(Cmd),

    {_, Runtime} = statistics(runtime), % 毫秒
    {_, WallClock} = statistics(wall_clock), % 毫秒

    cross:cast(?cross_type_server, ?MODULE, run_cb, [util:hostname(), CronId, Result, Runtime, WallClock]).

run_cb(Hostname, CronId, Result, Runtime, WallClock) ->
    case tbl_host:name_to_id(Hostname) of
        {ok, HostId} ->
            tbl_log_cron_run:add([{cron_id, CronId}, {host_id, HostId}, {time, util:unixtime()}, {result, Result}, {runtime, Runtime}, {wall_clock, WallClock}]);
        _ -> ?ERR("主机[~s]不存在")
    end.
