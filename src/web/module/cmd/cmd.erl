%% -*- coding: latin-1 -*-
-module(cmd).

-export([
    client_run/3
    ,client_run_async/2
    ,client_run_cb/4
]).

-include("cross.hrl").


%%%===================================================================
%%% API
%%%===================================================================

client_run(Id, LogDetailId, Cmd) ->
    cross_mgr:cast(node, Id, ?MODULE, client_run_async, [LogDetailId, Cmd]).

client_run_async(LogDetailId, Cmd0) ->
    Cmd = util:to_list(Cmd0),

    statistics(runtime),
    statistics(wall_clock),

    Result = os:cmd(Cmd),

    {_, Runtime} = statistics(runtime), % 毫秒
    {_, WallClock} = statistics(wall_clock), % 毫秒

    cross:cast(?cross_type_server, ?MODULE, client_run_cb, [LogDetailId, Result, Runtime, WallClock]).

client_run_cb(LogDetailId, Result, Runtime, WallClock) ->
    {ok, #{log_id := LogId}} = tbl_log_cmd_run_detail:get(LogDetailId),
    tbl_log_cmd_run:cnt_succ_add(LogId),
    tbl_log_cmd_run_detail:update(LogDetailId, [{result_time, util:unixtime()}, {result, Result}, {runtime, Runtime}, {wall_clock, WallClock}]),
    ok.
