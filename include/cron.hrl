%%% -*- coding: latin-1 -*-

%% 任务列表
-define(env_crons, crons).


%% 任务
-record(cron, {
    id = 0      % ID
    ,expr       % cron表达式
    ,cmd = ""   % 命令
}).

%% 任务cron表达式
-record(cron_expr, {
    minute
    ,hour
    ,day
    ,month
    ,week
}).
