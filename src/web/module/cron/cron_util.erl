%%% -*- coding: latin-1 -*-
-module(cron_util).

-export([
    parse_expr/1
    ,cron_get/0
    ,cron_add/3
    ,cron_del/1
]).

-include("common.hrl").
-include("cron.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% 解析cron表达式
-spec parse_expr(Expr :: binary()) -> #cron_expr{}.
parse_expr(Expr) ->
    Exprs = [ExprItem || ExprItem <- string:split(Expr, " ", all), ExprItem =/= <<>>],
    case length(Exprs) of
        5 ->
            try
                parse_expr2(Exprs)
            catch
                error:Reason  -> Reason
            end;
        _ -> {false, "cron表达式数目错误"}
    end.

cron_get() ->
    sys_env:get(?env_crons, []).

cron_add(CronId0, Expr, Cmd0) ->
    CronId = util:to_integer(CronId0),
    Cron = #cron{id = CronId, expr = Expr, cmd = util:to_list(Cmd0)},
    Crons= cron_get(),
    case lists:keytake(CronId, #cron.id, Crons) of
        false -> sys_env:save(?env_crons, [Cron | Crons]);
        {value, _Cron, CronsTmp} -> sys_env:save(?env_crons, [Cron | CronsTmp])
    end,
    ok.

cron_del(CronId0) ->
    CronId  = util:to_integer(CronId0),
    Crons   = cron_get(),
    sys_env:save(?env_crons, lists:keydelete(CronId, #cron.id, Crons)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_expr2([Minute, Hour, Day, Month, Week]) ->
    {ok, #cron_expr{
        minute = parse_expr_minute(Minute)
        ,hour = parse_expr_hour(Hour)
        ,day = parse_expr_day(Day)
        ,month = parse_expr_month(Month)
        ,week = parse_expr_week(Week)
    }}.

parse_expr_minute(Minute) ->
    Minute2 = parse_expr_common(Minute),
    if
        is_integer(Minute2) -> check_range(Minute2, 0, 59, "分钟 数据超出范围[0-59]");
        is_list(Minute2) -> [check_range(Item, 0, 59, "分钟 数据超出范围[0-59]") || Item <- Minute2];
        true -> Minute2
    end.

parse_expr_hour(Hour) ->
    Hour2 = parse_expr_common(Hour),
    if
        is_integer(Hour2) -> check_range(Hour2, 0, 23, "小时 数据超出范围[0-23]");
        is_list(Hour2) -> [check_range(Item, 0, 23, "小时 数据超出范围[0-23]") || Item <- Hour2];
        true -> Hour2
    end.

parse_expr_day(Day) ->
    Day2 = parse_expr_common(Day),
    if
        is_integer(Day2) -> check_range(Day2, 1, 31, "日期 数据超出范围[1-31]");
        is_list(Day2) -> [check_range(Item, 1, 31, "日期 数据超出范围[1-31]") || Item <- Day2];
        true -> Day2
    end.

parse_expr_month(Month) ->
    Month2 = parse_expr_common(Month),
    if
        is_integer(Month2) -> check_range(Month2, 1, 12, "月份 数据超出范围[1-12]");
        is_list(Month2) -> [check_range(Item, 1, 12, "月份 数据超出范围[1-12]") || Item <- Month2];
        true -> Month2
    end.

parse_expr_week(Week) ->
    Week2 = parse_expr_common(Week),
    if
        is_integer(Week2) -> check_range(Week2, 1, 7, "星期 数据超出范围[1-7]");
        is_list(Week2) -> [check_range(Item, 1, 7, "星期 数据超出范围[1-7]") || Item <- Week2];
        true -> Week2
    end.


parse_expr_common(<<"*">>) ->
    all;
parse_expr_common(Expr) ->
    case util_type:bitstring_to_term(Expr) of
        {ok, Val} -> Val;
        {error, Reason} ->
            erlang:error({false, Reason})
    end.

check_range(Val, Min, Max, _Msg) when Val >= Min, Val =< Max -> Val;
check_range(_Val, _Min, _Max, Msg) -> erlang:error({false, Msg}).
