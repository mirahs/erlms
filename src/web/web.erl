%% -*- coding: latin-1 -*-
-module(web).

-export([
    init/0
    ,menu_update/0

    ,echo_success/0
    ,echo_success/1
    ,echo_failed/1
    ,echo_json/1
]).

-include("common.hrl").
-include("web.hrl").


%%%===================================================================
%%% API
%%%===================================================================

init() ->
    ets:new(?ets_web_path2mfd,	[set, public, named_table, {read_concurrency,true}, {keypos, 1}]),
    ets:new(?ets_web_menu_data,	[set, public, named_table, {read_concurrency,true}, {keypos, 1}]),
    ets:new(?ets_web_menu_check,[set, public, named_table, {read_concurrency,true}, {keypos, 1}]),

    web_adm_menu:menu_init(),

    PrivDir		= code:priv_dir("."),
    StaticDir	= PrivDir ++ "/static/",
    erlweb:init(?web_port, [], StaticDir, ?web_session_app, web_erlweb_dispatch).

menu_update() ->
    ets:delete_all_objects(?ets_web_path2mfd),
    ets:delete_all_objects(?ets_web_menu_data),
    ets:delete_all_objects(?ets_web_menu_check),

    web_adm_menu:menu_init().


echo_success() ->
    echo_success([]).
echo_success(Data) ->
    ?MODULE:echo_json(#{code => 1, data => Data}).

echo_failed(Msg) ->
    ?MODULE:echo_json(#{code => 0, msg => util:to_binary(Msg)}).

echo_json(Data) ->
    {json, jsx:encode(Data)}.
