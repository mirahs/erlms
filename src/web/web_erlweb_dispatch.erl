%% -*- coding: latin-1 -*-
-module(web_erlweb_dispatch).

-export([
    get/4
]).

-include("web.hrl").


%%%===================================================================
%%% API
%%%===================================================================

get(Path, AppB, ModuleB, FuncB) ->
    #{module := Module, func := Func} = Handle =
        case ets:lookup(?ets_web_path2mfd, Path) of
            [{Path, PathHandle}] -> PathHandle;
            _ ->
                {ok, PathHandle} = erlweb_dispatch:get(Path, AppB, ModuleB, FuncB),
                ets:insert(?ets_web_path2mfd, {Path, PathHandle}),
                PathHandle
        end,
    case AppB of
        ?web_app_adm ->
            case web_adm_menu:menu_check(Path, Module, Func) of
                true -> {ok, Handle};
                false -> {error, ?web_url_noaccess}
            end;
        _ -> {ok, Handle}
    end.
