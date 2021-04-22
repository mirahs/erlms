%% -*- coding: latin-1 -*-
-module(tbl_log_cron_sync_detail).

-export([
    add/1
    ,delete/1
    ,delete_by_log_id/1
    ,get/1
    ,update/2
    ,all/0
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

add(Data) ->
    mysql:insert(?db_admin, log_cron_sync_detail, Data).

delete(Id) ->
    mysql:delete(?db_admin, log_cron_sync_detail, "`id`=?", [Id]).

delete_by_log_id(LogId) ->
    mysql:delete(?db_admin, log_cron_sync_detail, "`log_id`=?", [LogId]).

get(Id) ->
    mysql:select_map_row(?db_admin, "SELECT * FROM `log_cron_sync_detail` WHERE `id`=?", [Id]).

update(Id, Data) ->
    mysql:update(?db_admin, log_cron_sync_detail, Data, "`id`=?", [Id]).

all() ->
    mysql:select_map(?db_admin, "SELECT * FROM `log_cron_sync_detail`;").
