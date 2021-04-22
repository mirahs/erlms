%% -*- coding: latin-1 -*-
-module(tbl_log_adm_user_login).

-export([
    add/1
    ,delete/1
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

add(Data) ->
    mysql:insert(?db_admin, log_adm_user_login, Data).

delete(Id) ->
    mysql:delete(?db_admin, log_adm_user_login, "`id`=?", [Id]).
