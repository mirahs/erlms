%% -*- coding: latin-1 -*-
-module(tbl_host).

-export([
    add/1
    ,delete/1
    ,get/1
    ,update/2
    ,all/0
    ,name_to_id/1
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

add(Data) ->
    mysql:insert(?db_admin, host, Data).

delete(Id) ->
    mysql:delete(?db_admin, host, "`id`=?", [Id]).

get(Id) ->
    mysql:select_map_row(?db_admin, "SELECT * FROM `host` WHERE `id`=?", [Id]).

update(Id, Data) ->
    mysql:update(?db_admin, host, Data, "`id`=?", [Id]).

all() ->
    mysql:select_map(?db_admin, "SELECT * FROM `host`;").

name_to_id(Name) ->
    case mysql:select_row(?db_admin, "SELECT `id` FROM `host` WHERE `name`=?", [Name]) of
        {ok, [Id]} -> {ok, Id};
        _ -> false
    end.
