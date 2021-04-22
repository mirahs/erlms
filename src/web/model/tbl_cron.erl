%% -*- coding: latin-1 -*-
-module(tbl_cron).

-export([
    add/1
    ,delete/1
    ,get/1
    ,update/2
    ,all/0
    ,all_not_del/0
    ,name_to_id/1
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

add(Data) ->
    mysql:insert(?db_admin, cron, Data).

delete(Id) ->
    mysql:delete(?db_admin, cron, "`id`=?", [Id]).

get(Id) ->
    mysql:select_map_row(?db_admin, "SELECT * FROM `cron` WHERE `id`=?", [Id]).

update(Id, Data) ->
    mysql:update(?db_admin, cron, Data, "`id`=?", [Id]).

all() ->
    mysql:select_map(?db_admin, "SELECT * FROM `cron`;").

all_not_del() ->
    mysql:select_map(?db_admin, "SELECT * FROM `cron` WHERE `is_del`=0;").

name_to_id(Name) ->
    case mysql:select_row(?db_admin, "SELECT `id` FROM `cron` WHERE `name`=?", [Name]) of
        {ok, [Id]} -> {ok, Id};
        _ -> false
    end.
