%% -*- coding: latin-1 -*-
-module(util_list).

-export([
    implode/2
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% @doc 用Elem为分隔符合并list
%% @spec implode(List, Elem) -> MergeList;
%% List -> [A1, ..., An]
%% MergeList -> [A1] | [A1, Elem, A2] | [A1, Elem, A2, Elem, A3 ..., An]
implode([], _) ->
    [];
implode([E], _) ->
    [E];
implode([H | List], Elem) ->
    implode(List, Elem, [H]).

implode([H], Elem, Result) ->
    lists:reverse([H, Elem | Result]);
implode([H | T], Elem, Result) ->
    implode(T, Elem, [H, Elem | Result]).
