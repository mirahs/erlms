-module(web_erlydtl_tag).

-export([
    web_static_url/1

    ,ymdhis/1
    ,ymd/1
]).

-include("web.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% web 资源地址
web_static_url(_Args) ->
    ?web_static_url.


%% 格式化时间
ymdhis([Unixtime]) when is_integer(Unixtime) andalso Unixtime > 0 ->
    util:date_sep_YmdHis(Unixtime);
ymdhis(_Args) ->
    "1970-01-01 00:00:00".

%% 格式化日期
ymd([Unixtime]) when is_integer(Unixtime) andalso Unixtime > 0 ->
    util:date_sep_Ymd(Unixtime);
ymd(_Args) ->
    "1970-01-01".
