%% -*- coding: latin-1 -*-
-module(controller_adm_home).

-compile(nowarn_export_all).
-compile(export_all).

-include("common.hrl").
-include("web.hrl").


%% 欢迎
welcome(_Method, _Req, _OPts) ->
    {text, "welcome"}.

%% 菜单更新
menu_update(_Method, Req, _OPts) ->
    Data = cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"act">>, Data) of
        <<"update">> ->
            web:menu_update(),
            {error, "菜单更新成功"};
        _ -> {dtl}
    end.
