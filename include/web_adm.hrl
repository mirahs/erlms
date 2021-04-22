%%% -*- coding: latin-1 -*-

%% webssh 自动登录 url
-define(adm_webssh_url, "http://test01.gank.com:4001?hostname=~s&port=~w&username=~s&password=~s").


%% 后台账号类型
-define(adm_user_type_admin,	10). % 管理员
-define(adm_user_type_guest,	20). % 游客

-define(adm_user_types_desc, #{
    ?adm_user_type_admin    => "管理员",
    ?adm_user_type_guest    => "游客"
}).


%% 不需要检查的 控制器 和 方法
-define(adm_non_check_cv,		[
    {index, index}
    ,{index, login}
    ,{index, logout}
    ,{index, noaccess}
]).


-define(adm_ip_count,				5).
-define(adm_ips_count,				20).


-define(adm_cron_sync_state_begin,  1).
-define(adm_cron_sync_state_succ,   2).
-define(adm_cron_sync_state_failed, 3).

-define(adm_cron_sync_states, #{
    ?adm_cron_sync_state_begin  => "同步开始",
    ?adm_cron_sync_state_succ   => "同步成功",
    ?adm_cron_sync_state_failed => "同步失败"
}).
