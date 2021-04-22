%% -*- coding: latin-1 -*-
-module(tbl_log_cron_sync).

-export([
    add/1
    ,delete/1
    ,get/1
    ,update/2
    ,all/0
    ,cnt_succ_add/1
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

add(Data) ->
    mysql:insert(?db_admin, log_cron_sync, Data).

delete(Id) ->
    mysql:delete(?db_admin, log_cron_sync, "`id`=?", [Id]).

get(Id) ->
    mysql:select_map_row(?db_admin, "SELECT * FROM `log_cron_sync` WHERE `id`=?", [Id]).

update(Id, Data) ->
    mysql:update(?db_admin, log_cron_sync, Data, "`id`=?", [Id]).

all() ->
    mysql:select_map(?db_admin, "SELECT * FROM `log_cron_sync`;").

cnt_succ_add(LogId) ->
    mysql:execute(?db_admin, "UPDATE `log_cron_sync` SET `cnt_succ`=`cnt_succ`+1 WHERE `id`=?", [LogId]).
