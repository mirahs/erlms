%% -*- coding: latin-1 -*-
-module(tbl_stat_mem).

-export([
    add/1
    ,delete/1
    ,get/1
    ,get/2
    ,update/2
    ,all/0
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

add(Data) ->
    mysql:insert(?db_admin, stat_mem, Data).

delete(Id) ->
    mysql:delete(?db_admin, stat_mem, "`id`=?", [Id]).

get(Id) ->
    mysql:select_map_row(?db_admin, "SELECT * FROM `stat_mem` WHERE `id`=?", [Id]).

get(HostId, Num) ->
    mysql:select(?db_admin, "SELECT `hi_key`,`mem_total`,`mem_used` FROM `stat_mem` WHERE `host_id`=? ORDER BY `id` DESC LIMIT " ++ util:to_list(Num), [HostId]).

update(Id, Data) ->
    mysql:update(?db_admin, stat_mem, Data, "`id`=?", [Id]).

all() ->
    mysql:select_map(?db_admin, "SELECT * FROM `stat_mem`;").
