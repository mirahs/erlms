%% -*- coding: latin-1 -*-
-module(tbl_stat_load).

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
    mysql:insert(?db_admin, stat_load, Data).

delete(Id) ->
    mysql:delete(?db_admin, stat_load, "`id`=?", [Id]).

get(Id) ->
    mysql:select_map_row(?db_admin, "SELECT * FROM `stat_load` WHERE `id`=?", [Id]).

get(HostId, Num) ->
    mysql:select(?db_admin, "SELECT `hi_key`,`load1`,`load5`,`load15` FROM `stat_load` WHERE `host_id`=? ORDER BY `id` DESC LIMIT " ++ util:to_list(Num), [HostId]).

update(Id, Data) ->
    mysql:update(?db_admin, stat_load, Data, "`id`=?", [Id]).

all() ->
    mysql:select_map(?db_admin, "SELECT * FROM `stat_load`;").
