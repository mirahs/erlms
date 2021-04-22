%% -*- coding: latin-1 -*-
-module(dev).

-export([
    up/0
    ,up/1
    ,up/2
]).

-include_lib("kernel/include/file.hrl").
-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% 热更新所有模块(非强制)
up() ->
    do_up([], false).

%% 热更新所有模块(强制更新)
up(force) ->
    do_up([], true);

%% 热更新指定模块(非强制)
up(ModList) when is_list(ModList) ->
    do_up(ModList, false).

%% 热更新指定模块(强制更新)
up(ModList, force) when is_list(ModList) ->
    do_up(ModList, true).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 热更新
do_up(L, F) ->
    io:format("--- begin hot swapping node: ~p~n", [node()]),
    Args =
        case {L, F} of
            {[], false}         -> [];
            {[], true}          -> [force];
            {[_H | _T], false}  -> [L];
            {[_H | _T], true}   -> [L, force]
        end,

    Rtn = rpc:call(node(), sys_code, up, Args, infinity),
    print_up(Rtn).

%% 显示更新结果
print_up([]) -> ok;
print_up([{M, R} | T]) ->
    case R of
        ok -> io:format("# load module success: ~p~n", [M]);
        {error, Reason} -> io:format("* load module failed [~p]: ~p~n", [M, Reason])
    end,
    print_up(T).
