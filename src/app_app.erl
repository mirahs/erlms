%% -*- coding: latin-1 -*-
-module(app_app).

-behaviour(application).

-export([
    start/2
    ,stop/1
]).

-include("common.hrl").
-include("cross.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start(_StartType, _StartArgs) ->
    check_detached(),
    case init:get_plain_arguments() of
        [DirVar | T] ->
            gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {sys_alarm_handler, null}),
            error_logger:logfile({open, lists:concat([DirVar, "app_error.log"])}),
            % 热更管理器初始化
            sys_code:init(),

            #{type := Type} = app_conf:node(node(), ?salt_conf),
            start_before(Type),
            Mod = util:to_atom("sup_" ++ util:to_list(Type)),
            Mod:start_link([DirVar | T]);
        _ -> {error, args_error}
    end.

stop(_State) ->
    erlweb:stop(),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(debug).
check_detached() -> ok.
-else.
check_detached() ->
    case init:get_argument(noshell) =:= error orelse init:get_argument(noinput) =:= error of
        true -> erlang:error(not_detached);
        false -> ok
    end.
-endif.

start_before(?cross_type_server) ->
    % MySQL 初始化
    mysql:add_pool(?db_admin, ?mysql_username, ?mysql_password, ?mysql_database);
start_before(_Type) -> ok.
