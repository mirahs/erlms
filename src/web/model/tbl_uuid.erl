%% -*- coding: latin-1 -*-
-module(tbl_uuid).

-export([
    add/1
    ,update/2
    ,all/0
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

add(Data) ->
    mysql:insert(?db_admin, uuid, Data).

update(Key, Data) ->
    mysql:update(?db_admin, uuid, Data, "`key`=?", [Key]).

all() ->
    mysql:select_map(?db_admin, "SELECT * FROM `uuid`;").
