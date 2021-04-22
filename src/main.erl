%% -*- coding: latin-1 -*-
-module(main).

-export([
    start/0
    ,stop/0
    ,up/0

    ,stop_node/0
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% shell 启动服务器
start() ->
    #{cookie := Cookie} = app_conf:node(node(), ?salt_conf),
    erlang:set_cookie(node(), Cookie),

    {ok, _} = application:ensure_all_started(app),
    ok.

%% shell 热更服务器
up() ->
    [Node0] = init:get_plain_arguments(),
    Node	= util:to_atom(Node0),

    #{cookie := Cookie} = app_conf:node(Node, ?salt_conf),
    erlang:set_cookie(node(), Cookie),

    rpc:call(Node, dev, up, []),

    erlang:halt().

%% shell 关闭服务器
stop() ->
    [Node0] = init:get_plain_arguments(),
    Node	= util:to_atom(Node0),

    #{cookie := Cookie} = app_conf:node(Node, ?salt_conf),
    erlang:set_cookie(node(), Cookie),

    Res = rpc:call(Node, main, stop_node, []),
    ?INFO("stop node ~w Res:~p", [Node, Res]),

    erlang:halt().


%% 关闭节点
stop_node() ->
    ?INFO("stop_node begin..."),
    application:stop(app),
    ?INFO("stop_node end..."),
    erlang:halt().
