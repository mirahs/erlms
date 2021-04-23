%% -*- coding: latin-1 -*-
-module(app_conf).

-export([
    node/2
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

node(Node, Salt) ->
    try
        node2(Node, Salt)
    catch
        _:_ -> undefined
    end.

%% 节点配置(开发调试)
node2('erlms_server@127.0.0.1', ?salt_conf) ->
    #{type => server, cookie => 'adfiDIEH4753477&&999'};
node2('erlms_client@127.0.0.1', ?salt_conf) ->
    #{type => client, cookie => 'adfiDIEH4753477xx999'}.
