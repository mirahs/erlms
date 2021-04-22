%% -*- coding: latin-1 -*-
-module(tbl_cmd).

-export([
    add/1
    ,delete/1
    ,get/1
    ,update/2
    ,all/0
    ,all_not_del/0
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

add(Data) ->
    mysql:insert(?db_admin, cmd, Data).

delete(Id) ->
    mysql:delete(?db_admin, cmd, "`id`=?", [Id]).

get(Id) ->
    mysql:select_map_row(?db_admin, "SELECT * FROM `cmd` WHERE `id`=?", [Id]).

update(Id, Data) ->
    mysql:update(?db_admin, cmd, Data, "`id`=?", [Id]).

all() ->
    mysql:select_map(?db_admin, "SELECT * FROM `cmd`;").

all_not_del() ->
    mysql:select_map(?db_admin, "SELECT * FROM `cmd` WHERE `is_del`=0;").
