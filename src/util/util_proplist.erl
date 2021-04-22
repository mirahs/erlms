%% -*- coding: latin-1 -*-
-module(util_proplist).

-export([
    find_vals/2
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% 查找多个
find_vals(Key, List) ->
    find_vals(Key, List, []).
find_vals(_Key, [], Back) -> lists:reverse(Back);
find_vals(Key, [{Key, Val} | T], Back) ->
    find_vals(Key, T, [Val | Back]);
find_vals(Key, [_ | T], Back) ->
    find_vals(Key, T, Back).
