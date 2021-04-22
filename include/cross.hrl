%%% -*- coding: latin-1 -*-

-define(cross_type_server, server).     % 跨服类型-服务器
-define(cross_type_client, client).     % 跨类类型-客户端

-define(ets_cross_srv_online,       ets_cross_srv_online).

-define(env_cross,                  env_cross).


%% 跨服节点连接数据
-record(cross_srv, {
    id              % 主机ID

    ,node           % 节点(client@主机名) atom()

    ,pid            % 进程ID
    ,timer_ref      % 重连定时器

    ,mpid           % 镜像进程监视的pid
    ,mref           % 镜像进程监视器
}).
