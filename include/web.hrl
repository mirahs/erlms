%%% -*- coding: latin-1 -*-

%%%===================================================================
%%% ets
%%%===================================================================

-define(ets_web_path2mfd,	ets_web_path2mfd).	% 路径转控制器和方法
-define(ets_web_menu_data,	ets_web_menu_data).	% 账号对应菜单数据
-define(ets_web_menu_check,	ets_web_menu_check).	% 路径和账号对应访问权限缓存


%%%===================================================================
%%% erlweb
%%%===================================================================

-define(web_port,           	    3001).                      % web 端口

-define(web_project_name,			"erlms").		            % 项目名称
-define(web_corp_name,		        "干货科技").			    % 公司名称
-define(web_corp_url,		        "http://www.gank.top").		% 公司官网
-define(web_studio_name,	        "erlms").			        % 工作室名称
-define(web_studio_url,		        "http://www.erlms.gank.top").% 工作室官网


-define(web_app_adm,			    <<"adm">>).		            % app-adm
-define(web_session_app,    	    [?web_app_adm]).            % 需要 session 处理的 app 列表

%% web 资源地址
-define(web_static_url,				"/static/"). % http://res.read.gank.com/

-define(web_url_login,		        "/adm/index/login").
-define(web_url_index,		        "/adm/index/index").
-define(web_url_noaccess,		    "/adm/index/noaccess").


-define(web_get,					<<"GET">>).
-define(web_post,					<<"POST">>).

-define(web_error(Msg),				erlang:error({error, Msg})).
-define(web_error(Msg, Url),		erlang:error({error, Msg, Url})).
-define(web_failed(Msg),		    erlang:error(web:echo_failed(Msg))).
-define(web_redirect,				erlang:error({redirect})).
-define(web_redirect(Url),			erlang:error({redirect, Url})).

-record(web_path2mfd, {
    path,
    controller,
    module,
    func,
    dtl,
    dtle
}).
