%% -*- coding: latin-1 -*-
-module(controller_adm_index).

-compile(nowarn_export_all).
-compile(export_all).

-include("common.hrl").
-include("web.hrl").


%% 主页
index(_Method, _Req, _OPts) ->
    case web_adm:check_login() of
        true ->
            Type = web_adm:get_type(),
            Menus = web_adm_menu:menus(Type),
            {dtl, [
                {web_project_name, ?web_project_name}

                ,{menus, Menus}

                ,{account, web_adm:get_account()}
                ,{user_type_name, web_adm:get_type_name()}
            ]};
        false -> {redirect, ?web_url_login}
    end.

%% 登陆
login(?web_get, Req, _Opts) ->
    case web_adm:check_login() of
        true -> {redirect, ?web_url_index, Req};
        false -> {dtl, [{web_project_name, ?web_project_name}]}
    end;
login(?web_post, Req, _Opts) ->
    case web_adm:login(Req) of
        {ok} -> web:echo_success();
        {error, ErrorMsg} -> web:echo_failed(ErrorMsg)
    end.

%% 退出
logout(_Method, Req, _Opts) ->
    Req2 = erlweb_session:destory(Req),
    {redirect, ?web_url_login, Req2}.

%% 无权限访问
noaccess(_Method, _Req, _Opts) ->
    {dtl, [{account, web_adm:get_account()}]}.
