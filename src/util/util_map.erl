%% -*- coding: latin-1 -*-
-module(util_map).

-export([
    exists/3
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

exists(Key, Val, Maps) ->
    Fun = fun(Map) -> maps:get(Key, Map) =:= Val end,
    lists:any(Fun, Maps).
