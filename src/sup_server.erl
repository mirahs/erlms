%% -*- coding: latin-1 -*-
-module(sup_server).

-behaviour(supervisor).

-export([
    start_link/1

    ,init/1
]).

-include("common.hrl").
-include("cross.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    ?INFO("[~w] 正在启动监控树...", [?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


%%%===================================================================
%%% supervisor callback functions
%%%===================================================================

init(Args) ->
    %% core
    List1 = get_core(Args),
    %% 逻辑模块
    List2 = get_mod(Args),
    {ok, {{one_for_one, 50, 1}, boot_misc:swap_sup_child(List1 ++ List2)}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 核心模块
get_core([DirVar | _]) ->
    WebArg = web:init(),
    [
        {logger_file, {logger_file, start_link, [DirVar]}}
        ,{sys_env, {sys_env, start_link, [[?MODULE, ?cross_type_server, DirVar]]}}
        ,{uuid_mgr, {uuid_mgr, start_link, []}}
        ,{erlweb_sup, {erlweb_sup, start_link, [WebArg#{dir_var => DirVar}]}, permanent, infinity, supervisor, [erlweb_sup]}
    ].

%% 逻辑模块
get_mod(_Args) ->
    [
        {cross_mgr, {cross_mgr, start_link, []}}
    ].
