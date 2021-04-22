%% -*- coding: latin-1 -*-
-module(util_math).

-export([
    rand/1
    ,rand/2
    ,float/2
    ,float_binary/2
]).


%%%===================================================================
%%% API
%%%===================================================================

%% 返回一个 1 到 N 之间的整数
%% N >= 1, 1 <= X <= N
-spec rand(N :: pos_integer()) -> X :: pos_integer().
rand(N) ->
    X = rand:uniform(N),
    X.

%% 返回一个 Min 到 Max 之间的整数
%% Min >= 1, Min <= Max, Min <= X <= Max
-spec rand(Min :: pos_integer(), Max :: pos_integer()) -> X :: pos_integer().
rand(Min, Min) -> Min;
rand(Min, Max) ->
    M = Min - 1,
    X = rand:uniform(Max - M) + M,
    X.

%% 保留X位小数
float(FloatNumber, X) ->
    N = math:pow(10, X),
    round(FloatNumber * N) / N.

float_binary(FloatNumber, X) ->
    float_to_binary(FloatNumber, [{decimals, X}]).
