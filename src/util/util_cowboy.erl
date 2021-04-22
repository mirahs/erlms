%% -*- coding: latin-1 -*-
-module(util_cowboy).

-export([
    ip/1
    ,get_values/2
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

ip(Req) ->
%%    ?DEBUG("cowboy_req:peer:~p", [cowboy_req:peer(Req)]),
%%    ?DEBUG("cowboy_req:header x-real-ip:~p", [cowboy_req:header(<<"x-real-ip">>, Req)]),
%%    ?DEBUG("cowboy_req:headers:~p", [cowboy_req:headers(Req)]),
    case cowboy_req:header(<<"x-real-ip">>, Req) of
        undefined ->
            {{Ip0, Ip1, Ip2, Ip3}, _Port} = cowboy_req:peer(Req),
            lists:concat([Ip0, ".", Ip1, ".", Ip2, ".", Ip3]);
        Ip -> util:to_list(Ip)
    end.

%% 获取值数组
get_values(Key0, List) ->
    Key = util:to_binary(util:to_list(Key0) ++ "[]"),
    util_proplist:find_vals(Key, List).
