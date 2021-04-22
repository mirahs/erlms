%% -*- coding: latin-1 -*-
-module(web_adm).

-export([
    check_login/0
    ,login/1

    ,get_id/0
    ,get_account/0
    ,get_type/0
    ,get_type_name/0
    ,get_type_name/1
]).

-include("common.hrl").
-include("web.hrl").
-include("web_adm.hrl").

-define(session_id,         session_id).
-define(session_account,    session_account).
-define(session_password,   session_password).
-define(session_type,       session_type).
-define(session_hash,       session_hash).


%%%===================================================================
%%% API
%%%===================================================================

%% 检查是否登录
check_login() ->
    UserName	= erlweb_session:get(?session_account),
    Password	= erlweb_session:get(?session_password),
    Hash		= erlweb_session:get(?session_hash),

    HashRule	= login_key_rule(UserName, Password),
    Hash =:= HashRule.

%% 登录
login(Req) ->
    {ok, Data, _Req2} = cowboy_req:read_urlencoded_body(Req),
    IP      = util_cowboy:ip(Req),
    Account = proplists:get_value(<<"account">>, Data),
    Password= proplists:get_value(<<"password">>, Data),
    case tbl_adm_user:get_by_account(Account) of
        {ok, AdmUser = #{id := IdDb, password := PasswordDb, type := TypeDb}} ->
            case util:to_binary(util:md5(Password)) of
                PasswordDb ->
                    Hash = login_key_rule(Account, Password),
                    erlweb_session:set([
                        {?session_id,			IdDb}
                        ,{?session_account,		Account}
                        ,{?session_password,	Password}
                        ,{?session_type,		TypeDb}
                        ,{?session_hash,		Hash}
                    ]),
                    login_update(AdmUser, IP),
                    login_log(Account, IP, ?true),
                    {ok};
                _ ->
                    login_log(Account, IP, ?false),
                    {error, "密码错误"}
            end;
        _ ->
            login_log(Account, IP, ?false),
            {error, "账号不存在"}
    end.


%% 账号ID
get_id() ->
    erlweb_session:get(?session_id, 0).

%% 帐号
get_account() ->
    erlweb_session:get(?session_account, "").

%% 帐号类型
get_type() ->
    erlweb_session:get(?session_type, 0).

%% 账号类型名称
get_type_name() ->
    get_type_name(get_type()).
get_type_name(Type) ->
    maps:get(Type, ?adm_user_types_desc, "").


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 登录 hash
login_key_rule(Account, Password) ->
    crypto:hash(sha, util:to_list(Account) ++ util:to_list(Password)).

%% 登录更新
login_update(#{id := Id, login_times := LoginTimes}, Ip) ->
    tbl_adm_user:update(Id, [{login_times, LoginTimes + 1}, {login_time, util:unixtime()}, {login_ip, Ip}]).

%% 登录日志记录
login_log(Account, Ip, Status) ->
    tbl_log_adm_user_login:add([{account, Account}, {time, util:unixtime()}, {status, Status}, {ip, Ip}, {ip_segment, ""}, {address, ""}]).
