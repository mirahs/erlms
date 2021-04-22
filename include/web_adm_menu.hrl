%%% -*- coding: latin-1 -*-

%% 菜单
-define(web_menus, [
    #{code => home,     name => "首页",       data => ?web_menu_home},
    #{code => sys,      name => "管理",       data => ?web_menu_sys},
    #{code => sa,       name => "系统",       data => ?web_menu_sa}
]).


%% 菜单-首页
-define(web_menu_home, [
    #{code => welcome,      name => "欢迎",           url => "home/welcome",      key => [10,20]},
    #{code => menu_update,  name => "菜单更新",       url => "home/menu_update",  key => [10]}
]).

%% 菜单-管理
-define(web_menu_sys, [
    #{code => password,     name => "密码更新",     key => [10,20]},
    #{name => "管理员管理", data => [
        #{code => master_new,       name => "添加管理员",        key => [10]},
        #{code => master_list,      name => "管理员列表",        key => [10]}
    ]},
    #{name => "日志", data => [
        #{code => log_login,        name => "登录日志",         url => "sys/log_login",     key => [10]}
    ]}
]).

%% 菜单-系统
-define(web_menu_sa, [
    #{code => server,       name => "服务端",      key => [10]},
    #{code => host,         name => "主机",         key => [10]},
    #{name => "统计", data => [
        #{code => stat_summary,     name => "汇总",           key => [10]},
        #{code => stat_load,        name => "负载",           key => [10]},
        #{code => stat_cpu_used,    name => "cpu使用率",      key => [10]},
        #{code => stat_mem,         name => "内存",           key => [10]}
    ]},
    #{name => "命令", data => [
        #{code => cmd,              name => "命令",           key => [10]},
        #{code => cmd_run,          name => "命令执行",      key => [10]},
        #{code => cmd_run_log,     name => "命令执行日志",    key => [10]},
        #{code => cmd_run_log_detail,   name => "命令执行日志详情",  key => [10]}
    ]},
    #{name => "任务", data => [
        #{code => cron,             name => "任务列表",       key => [10]},
        #{code => cron_sync,        name => "任务同步",      key => [10]},
        #{code => cron_sync_log,    name => "任务同步日志",  key => [10]},
        #{code => cron_sync_log_detail, name => "任务同步日志详情",  key => [10]},
        #{code => cron_run_log,     name => "任务执行日志",  key => [10]}
    ]}
]).
