%% -*- coding: latin-1 -*-
-module(util).

-export([
    md5/1
    ,cn/1
    ,fbin/1
    ,fbin/2
    ,hostname/0

    ,to_atom/1
    ,to_list/1
    ,to_binary/1
    ,to_float/1
    ,to_integer/1
    ,to_tuple/1

    ,list_to_string/4
    ,list_to_atom/1

    ,unixtime/0
    ,unixtime/1
    ,date/0
    ,time/0
    ,now/0
    ,localtime/0
    ,unixtime2localtime/1
    ,date_sep_Ymd/0
    ,date_sep_Ymd/1
    ,date_sep_YmdHis/0
    ,date_sep_YmdHis/1
    ,time_sep_hi5/0
    ,time_format/1
]).

-include("common.hrl").

%% 0000 到 1970 年的秒数
-define(diff_seconds_0000_1970, 62167219200).


%%%===================================================================
%%% API
%%%===================================================================

md5(S) ->
    binary_to_list(list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(S))])).

%% 在控制台显示带中文的字符串
cn(Str) ->
    io:format("~ts", [iolist_to_binary(io_lib:format(Str, []))]).

fbin(Format) ->
    fbin(Format, []).

fbin(Format, Args) ->
    list_to_binary(io_lib:format(Format, Args)).

%% 获取 hostname
-spec hostname() -> binary().
hostname() ->
    case sys_env:get(node_hostname) of
        undefined ->
            Node = to_list(erlang:node()),
            [_, Hostname]= re:split(Node, "@"),
            sys_env:set(node_hostname, Hostname),
            Hostname;
        Hostname -> Hostname
    end.


%%%===================================================================
%%% 类型转换
%%%===================================================================

%% atom
to_atom(Msg) when is_atom(Msg) ->
    Msg;
to_atom(Msg) when is_binary(Msg) ->
    ?MODULE:list_to_atom(binary_to_list(Msg));
to_atom(Msg) when is_integer(Msg) ->
    ?MODULE:list_to_atom(integer_to_list(Msg));
to_atom(Msg) when is_tuple(Msg) ->
    ?MODULE:list_to_atom(tuple_to_list(Msg));
to_atom(Msg) when is_list(Msg) ->
    Msg2 = list_to_binary(Msg),
    Msg3 = binary_to_list(Msg2),
    ?MODULE:list_to_atom(Msg3);
to_atom(_) ->
    ?MODULE:list_to_atom("").

%% list
to_list(Msg) when is_list(Msg) ->
    Msg;
to_list(Msg) when is_atom(Msg) ->
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) ->
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) ->
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) ->
    float_to_list(Msg);
to_list(_) ->
    [].

%% binary
to_binary(Msg) when is_binary(Msg) ->
    Msg;
to_binary(Msg) when is_atom(Msg) ->
    list_to_binary(atom_to_list(Msg));
to_binary(Msg) when is_list(Msg) ->
    list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) ->
    list_to_binary(integer_to_list(Msg));
to_binary(_Msg) ->
    <<>>.

%% float
to_float(Msg)->
    Msg2 = to_list(Msg),
    list_to_float(Msg2).

%% integer
to_integer(Msg) when is_integer(Msg) ->
    Msg;
to_integer(Msg) when is_binary(Msg) ->
    Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) ->
    list_to_integer(Msg);
to_integer(_Msg) ->
    0.

%% tuple
to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) -> {T}.


%%%===================================================================
%%% 系统加强
%%%===================================================================

%% 数组转成字符串
%% List -> String
%% H 附加在开头
%% M 夹在中间
%% T 附加在尾部
list_to_string([], _H, _M, _T) ->
    [];
list_to_string([HList|TList], H, M, T) ->
    list_to_string(TList, H, M, T, H ++ to_list(HList)).

list_to_string([], _H, _M, T, Str) ->
    Str ++ T;
list_to_string([HList|TList], H, M, T, Str) ->
    list_to_string(TList, H, M, T, Str ++ M ++ to_list(HList)).

%% list 转 atom 优化
list_to_atom(List)->
    try
        erlang:list_to_existing_atom(List)
    catch _:_ ->
        erlang:list_to_atom(List)
    end.


%%%===================================================================
%%% 时间相关
%%%===================================================================

unixtime() ->
    {M, S, _Ms} = ?MODULE:now(),
    M * 1000000 + S.

%% @spec unixtime(ms) -> Timestamp
%% Timestamp = integer()
%% @doc 取得当前的unix时间戳，精确到毫秒
unixtime(ms) ->
    {S1, S2, S3} = ?MODULE:now(),
    trunc(S1 * 1000000000 + S2 * 1000 + S3 / 1000).

% {Y, M, D}
date() ->
    {Date, _Time} = calendar:now_to_local_time(?MODULE:now()),
    Date.

% {H, I, S}
time() ->
    {_Date, Time} = calendar:now_to_local_time(?MODULE:now()),
    Time.

now() ->
    os:timestamp().

% {{Y,M,D}, {H,I,S}}
localtime() ->
    calendar:now_to_local_time(?MODULE:now()).

%% 根据1970年以来的秒数获得日期
unixtime2localtime(Unixtime) ->
    Unixtime2= util:to_integer(Unixtime),
    DateTime = calendar:gregorian_seconds_to_datetime(Unixtime2 + ?diff_seconds_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

%% 年月日
date_sep_Ymd() ->
    date_sep_Ymd(unixtime()).

date_sep_Ymd(Unixtime) ->
    {{Y, M, D}, _Time} = unixtime2localtime(Unixtime),
    to_list(Y) ++ "-" ++ time_format(M) ++ "-" ++ time_format(D).

%% 年月日时分秒
date_sep_YmdHis() ->
    date_sep_YmdHis(unixtime()).

date_sep_YmdHis(Unixtime) ->
    {{Y, M, D}, {H, I, S}} = unixtime2localtime(Unixtime),
    util:to_list(Y) ++ "-" ++ util:time_format(M) ++ "-" ++ util:time_format(D) ++ " " ++ util:time_format(H) ++ ":" ++  util:time_format(I) ++ ":" ++ util:time_format(S).

%% 每五分钟递增
time_sep_hi5() ->
    {H, I, _S} = ?MODULE:time(),
    time_format(H) ++ ":" ++time_format((I div 5) * 5).

time_format(D) ->
    case D < 10 of
        true -> "0" ++ to_list(D);
        false -> to_list(D)
    end.
