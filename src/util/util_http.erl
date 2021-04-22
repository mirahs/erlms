%%% -*- coding: latin-1 -*-
-module(util_http).

-export([
    kvs2param/1

    ,ibrowse_get/1
    ,ibrowse_get/2
    ,ibrowse_get2/2
    ,ibrowse_post/2
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

kvs2param([]) -> <<"">>;
kvs2param(Kvs) ->
    Fun = fun({K, V}, BinAcc) -> <<BinAcc/binary, ?B(K), "=", ?B(V), "&">> end,
    Str = lists:foldl(Fun, <<>>, Kvs),
    binary:part(Str, 0, byte_size(Str) - 1).


ibrowse_get(Url) ->
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _ServerHeaders, Body} -> {ok, erlang:iolist_to_binary(Body)};
        {ok, _Status, _ServerHeaders, _Body} -> {false, "unknown error"};
        {error, Error} -> {false, Error}
    end.

ibrowse_get(Url, SendData) ->
    QsData = kvs2param(SendData),
    case ibrowse:send_req(Url, [], get, QsData) of
        {ok, "200", _ServerHeaders, Body} -> {ok, erlang:iolist_to_binary(Body)};
        {ok, Status, _ServerHeaders, Body} -> {false, ibrowse_format_error(Status, Body)};
        {error, Error} -> {false, Error}
    end.

ibrowse_get2(Url2, SendData) ->
    QsData	= kvs2param(SendData),
    Url 	= util:to_list(<<?B(Url2), "?", QsData/binary>>),
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _ServerHeaders, Body} -> {ok, erlang:iolist_to_binary(Body)};
        {ok, Status, _ServerHeaders, Body} -> {false, ibrowse_format_error(Status, Body)};
        {error, Error} -> {false, Error}
    end.

ibrowse_post(Url, SendData) ->
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    case ibrowse:send_req(Url, Headers, post, kvs2param(SendData)) of
        {ok, "200", _ServerHeaders, Body} -> {ok, erlang:iolist_to_binary(Body)};
        {ok, Status, _ServerHeaders, Body} -> {false, ibrowse_format_error(Status, Body)};
        {error, Error} -> {false, Error}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

ibrowse_format_error(Status, ResponseBody) ->
    io_lib:format("unknown error Status:~w, ResponseBody:~s", [Status, ResponseBody]).
