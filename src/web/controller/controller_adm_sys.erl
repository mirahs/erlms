%% -*- coding: latin-1 -*-
-module(controller_adm_sys).

-compile(nowarn_export_all).
-compile(export_all).

-include("common.hrl").
-include("web.hrl").
-include("web_adm.hrl").


%% 密码更新
password(?web_get, Req, _Opts) ->
    Data    = cowboy_req:parse_qs(Req),
    Account0= proplists:get_value(<<"account">>, Data),
    Account = ?IF(Account0 =:= undefined, web_adm:get_account(), Account0),
    {dtl, [{account, Account}]};
password(?web_post, Req, _Opts) ->
    {ok, Data, _Req2} = cowboy_req:read_urlencoded_body(Req),
    Account = proplists:get_value(<<"account">>, Data, <<>>),
    Password= proplists:get_value(<<"password">>, Data, <<>>),
    ?IF(Account =:= <<>> orelse Password =:= <<>>, ?web_failed("请输入正确的数据"), skip),
    tbl_adm_user:update_by_account(Account, [{password, util:md5(util:md5(Password))}]),
    web:echo_success().

%% 添加管理员
master_new(?web_get, Req, _Opts) ->
    Data= cowboy_req:parse_qs(Req),
    Id  = proplists:get_value(<<"id">>, Data, <<>>),
    ?IF(Id =:= <<>>, skip, begin {ok, AdmUser} = tbl_adm_user:get(Id), erlweb_tpl:assign(data, AdmUser) end),

    Type= web_adm:get_type(),
    UserTypes = [{UT, UD} || {UT, UD} <- maps:to_list(?adm_user_types_desc),  UT >= Type],
    {dtl, [{user_types, UserTypes}]};
master_new(?web_post, Req, _Opts) ->
    {ok, Data, _Req2} = cowboy_req:read_urlencoded_body(Req),
    Id      = proplists:get_value(<<"id">>, Data, <<>>),
    Account = proplists:get_value(<<"account">>, Data, <<>>),
    Type    = proplists:get_value(<<"type">>, Data, <<>>),
    Remark  = proplists:get_value(<<"remark">>, Data, <<>>),
    ?IF(Account =:= <<>> orelse Type =:= <<>> orelse Remark =:= <<>>, ?web_failed("请输入正确的数据"), skip),
    Bind = [{account, Account}, {type, Type}, {remark, Remark}],
    case Id of
        <<>> ->
            Password = util:md5(util:md5("123456")),
            Bind2 = [{password, Password} | Bind],
            tbl_adm_user:add(Bind2);
        _ -> tbl_adm_user:update(Id, Bind)
    end,
    web:echo_success().

%% 管理员列表
master_list(?web_get, Req, _Opts) ->
    Data= cowboy_req:parse_qs(Req),
    Id  = util:to_integer(proplists:get_value(<<"id">>, Data)),
    case proplists:get_value(<<"act">>, Data) of
        <<"del">> ->
            case web_adm:get_id() of
                Id -> {error, "不能删除自己"};
                _ ->
                    tbl_adm_user:delete(Id),
                    {error, "删除成功"}
            end;
        <<"lock">> ->
            case web_adm:get_id() of
                Id -> {error, "不能操作自己"};
                _ ->
                    IsLocked0   = proplists:get_value(<<"is_locked">>, Data),
                    Locked      = ?IF(IsLocked0 =:= <<"1">>, 0, 1),
                    tbl_adm_user:update(Id, [{is_locked, Locked}]),
                    ?web_redirect
            end;
        _ ->
            #{page := Page, datas := Datas0} = web_page:page(Req, adm_user),
            Fun = fun
                      (#{type := TypeF, is_locked := IsLockedF} = DataF, DatasAcc) ->
                          TypeDesc      = maps:get(TypeF, ?adm_user_types_desc),
                          IsLockedDesc  = ?IF(IsLockedF =:= 0, "锁住", "解锁"),
                          [DataF#{type_desc => TypeDesc, is_locked_desc => IsLockedDesc} | DatasAcc]
                  end,
            Datas = lists:reverse(lists:foldl(Fun, [], Datas0)),
            {dtl, [{page, Page}, {datas, Datas}]}
    end.

%% 登录日志
log_login(?web_get, Req, _Opts) ->
    Data= cowboy_req:parse_qs(Req),
    Id  = proplists:get_value(<<"id">>, Data, <<>>),
    ?IF(Id =:= <<>>, skip, begin tbl_log_adm_user_login:delete(Id), ?web_redirect end),

    Account = proplists:get_value(<<"account">>, Data, <<>>),
    Wheres  = ?IF(Account =:= <<>>, [], begin erlweb_tpl:assign(account, Account), [{account, Account}] end),

    #{page := Page, datas := Datas0} = web_page:page(Req, log_adm_user_login, [], Wheres, "`id` DESC", []),
    Fun = fun
              (#{status := StatusF} = DataF, DatasAcc) ->
                  StatusDesc = ?IF(StatusF =:= 1, "成功", "失败"),
                  [DataF#{status_desc => StatusDesc} | DatasAcc]
          end,
    Datas = lists:reverse(lists:foldl(Fun, [], Datas0)),
    {dtl, [{page, Page}, {datas, Datas}]}.
