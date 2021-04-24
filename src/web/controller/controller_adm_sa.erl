%% -*- coding: latin-1 -*-
-module(controller_adm_sa).

-compile(nowarn_export_all).
-compile(export_all).

-include("common.hrl").
-include("web.hrl").
-include("web_adm.hrl").

-define(cmd_id_max_built_in,    3). % 最大内置命令(大于这个才可以编辑和删除)


%% 服务端
server(_Method, Req, _Opts) ->
    Data = cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"act">>, Data) of
        <<"update">> ->
            Result = os:cmd("git pull"),
            Result2= re:replace(Result, "\n", "<br />", [global, {return, list}]),
            {dtl, [{result, Result2}]};
        <<"reload">> ->
            Result = os:cmd("git pull && sh ctl.sh up_server"),
            Result2= re:replace(Result, "\n", "<br />", [global, {return, list}]),
            {dtl, [{result, Result2}]};
        <<"restart">> ->
            os:cmd("sh ctl.sh stop_server && sh ctl.sh start_server"),
            {error, "重启成功"};
        _ -> {dtl}
    end.


%% 主机
host(?web_get, Req, _Opts) ->
    Data= cowboy_req:parse_qs(Req),
    Id  = util:to_integer(proplists:get_value(<<"id">>, Data)),
    case proplists:get_value(<<"act">>, Data) of
        <<"ssh">> ->
            {ok, #{name := Hostname, ssh_port := SshPort, ssh_username := SshUsername, ssh_password := SshPassword}} = tbl_host:get(Id),

            Url = io_lib:format(?adm_webssh_url, [Hostname, SshPort, SshUsername, base64:encode(SshPassword)]),
            ?web_redirect(Url);
        <<"edit">> ->
            case Id of
                0 -> erlweb_tpl:assign(title, "添加主机");
                _ ->
                    erlweb_tpl:assign(title, "编辑主机"),
                    {ok, Host} = tbl_host:get(Id),
                    erlweb_tpl:assign(data, Host)
            end,
            {dtl_edit};
        <<"del">> ->
            tbl_host:delete(Id),
            cross_mgr:cross_del(Id),
            {error, "删除成功"};
        _ ->
            #{page := Page, datas := Datas0} = web_page:page(Req, host),
            Fun = fun(FData = #{id := FId}) -> FData#{status => cross_srv_online:check(FId)} end,
            Datas = lists:map(Fun, Datas0),
            {dtl, [{page, Page}, {datas, Datas}]}
    end;
host(?web_post, Req, _Opts) ->
    {ok, Data, _Req2} = cowboy_req:read_urlencoded_body(Req),
    Id      = proplists:get_value(<<"id">>, Data, <<>>),
    Name    = proplists:get_value(<<"name">>, Data, <<>>),
    SshPort0= proplists:get_value(<<"ssh_port">>, Data, <<>>),
    SshUser = proplists:get_value(<<"ssh_username">>, Data, <<>>),
    SshPass = proplists:get_value(<<"ssh_password">>, Data, <<>>),
    Remark  = proplists:get_value(<<"remark">>, Data, <<>>),
    ?IF(Name =:= <<>>, ?web_failed("请输入正确的数据"), skip),
    SshPort = ?IF(SshPort0 =:= <<>>, 22, SshPort0),
    Bind    = [{name, Name}, {ssh_port, SshPort}, {ssh_username, SshUser}, {ssh_password, SshPass}, {remark, Remark}],
    case Id of
        <<>> ->
            Bind2 = [{time, util:unixtime()} | Bind],
            {ok, _AffectedRows, LastInsertId} = tbl_host:add(Bind2),
            {ok, Host} = tbl_host:get(LastInsertId),
            cross_mgr:cross_add(Host);
        _ -> tbl_host:update(Id, Bind)
    end,
    web:echo_success().


%% 统计-汇总
stat_summary(?web_get, Req, _Opts) ->
    Data = cowboy_req:parse_qs(Req),

    {ok, Hosts} = tbl_host:all(),

    case proplists:get_value(<<"host_id">>, Data, <<>>) of
        <<>> -> {dtl, [{hosts, Hosts}, {ec_load, jsx:encode(#{})}, {ec_cpu_used, jsx:encode(#{})}, {ec_mem, jsx:encode(#{})}]};
        HostId ->
            Num0= proplists:get_value(<<"num">>, Data, <<>>),
            Num = ?IF(Num0 =:= <<>>, 10, Num0),

            Ecs = echarts_summary(HostId, Num),
            {dtl, [{host_id, HostId}, {num, Num}, {hosts, Hosts}] ++ Ecs}
    end.

%% 统计-负载
stat_load(?web_get, Req, _Opts) ->
    Data = cowboy_req:parse_qs(Req),

    {ok, Hosts} = tbl_host:all(),

    case proplists:get_value(<<"host_id">>, Data, <<>>) of
        <<>> -> {dtl, [{hosts, Hosts}, {ec, jsx:encode(#{})}]};
        HostId ->
            Num0= proplists:get_value(<<"num">>, Data, <<>>),
            Num = ?IF(Num0 =:= <<>>, 20, Num0),
            erlweb_tpl:assign(num, Num),

            {ok, Datas} = tbl_stat_load:get(HostId, Num),
            Ec = echarts_load(Datas),
            {dtl, [{host_id, HostId}, {hosts, Hosts}, {ec, jsx:encode(Ec)}]}
    end.

%% 统计-cpu使用率
stat_cpu_used(?web_get, Req, _Opts) ->
    Data = cowboy_req:parse_qs(Req),

    {ok, Hosts} = tbl_host:all(),

    case proplists:get_value(<<"host_id">>, Data, <<>>) of
        <<>> -> {dtl, [{hosts, Hosts}, {ec, jsx:encode(#{})}]};
        HostId ->
            Num0= proplists:get_value(<<"num">>, Data, <<>>),
            Num = ?IF(Num0 =:= <<>>, 20, Num0),
            erlweb_tpl:assign(num, Num),

            {ok, Datas} = tbl_stat_cpu_used:get(HostId, Num),
            Ec = echarts_cpu_used(Datas),
            {dtl, [{host_id, HostId}, {hosts, Hosts}, {ec, jsx:encode(Ec)}]}
    end.

%% 统计-内存
stat_mem(?web_get, Req, _Opts) ->
    Data = cowboy_req:parse_qs(Req),

    {ok, Hosts} = tbl_host:all(),

    case proplists:get_value(<<"host_id">>, Data, <<>>) of
        <<>> -> {dtl, [{hosts, Hosts}, {ec, jsx:encode(#{})}]};
        HostId ->
            Num0= proplists:get_value(<<"num">>, Data, <<>>),
            Num = ?IF(Num0 =:= <<>>, 20, Num0),
            erlweb_tpl:assign(num, Num),

            {ok, Datas} = tbl_stat_mem:get(HostId, Num),
            Ec = echarts_mem(Datas),
            {dtl, [{host_id, HostId}, {hosts, Hosts}, {ec, jsx:encode(Ec)}]}
    end.


%% 命令
cmd(?web_get, Req, _Opts) ->
    Data= cowboy_req:parse_qs(Req),
    Id  = util:to_integer(proplists:get_value(<<"id">>, Data)),
    case proplists:get_value(<<"act">>, Data) of
        <<"edit">> ->
            case Id of
                0 -> erlweb_tpl:assign(title, "添加命令");
                _ ->
                    erlweb_tpl:assign(title, "编辑命令"),
                    {ok, Host} = tbl_cmd:get(Id),
                    erlweb_tpl:assign(data, Host)
            end,
            {dtl_edit};
        <<"del">> ->
            %tbl_cmd:delete(Id),
            tbl_cmd:update(Id, [{is_del, ?true}]),
            {error, "删除成功"};
        _ ->
            #{page := Page, datas := Datas} = web_page:page_where(Req, cmd, [{is_del, ?false}]),
            {dtl, [{page, Page}, {datas, Datas}, {id_built_in, ?cmd_id_max_built_in}]}
    end;
cmd(?web_post, Req, _Opts) ->
    {ok, Data, _Req2} = cowboy_req:read_urlencoded_body(Req),
    Id      = proplists:get_value(<<"id">>, Data, <<>>),
    Name    = proplists:get_value(<<"name">>, Data, <<>>),
    Cmd     = proplists:get_value(<<"cmd">>, Data, <<>>),
    ?IF(Name =:= <<>> orelse Cmd =:= <<>>, ?web_failed("请输入正确的数据"), skip),
    Bind    = [{name, Name}, {cmd, Cmd}],
    case Id of
        <<>> ->
            Bind2 = [{time, util:unixtime()} | Bind],
            tbl_cmd:add(Bind2);
        _ -> tbl_cmd:update(Id, Bind)
    end,
    web:echo_success().

%% 命令执行
cmd_run(_Method, Req, _Opts) ->
    Data = cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"act">>, Data) of
        <<"data">> ->
            Page = #{data := Datas0} = web_page:page_table(Req, host),
            Fun = fun(FData = #{time := FTime}) -> FData#{time => util:to_binary(util:date_sep_YmdHis(FTime))} end,
            Datas = lists:map(Fun, Datas0),
            web:echo_json(Page#{data => Datas});
        <<"run">> ->
            IsAll = util:to_integer(proplists:get_value(<<"is_all">>, Data, 0)),
            Ids =
                case IsAll of
                    ?true -> cross_srv_online:get_ids();
                    ?false ->
                        IdsTmp = util_cowboy:get_values("ids", Data),
                        ?IF(IdsTmp =:= [], ?web_failed("请选择主机"), skip),
                        IdsTmp
                end,

            CmdId = proplists:get_value(<<"cmd_id">>, Data, <<>>),
            ?IF(CmdId =:= <<>>, ?web_failed("请选择要执行的命令"), skip),
            {ok, #{cmd := Cmd}} = tbl_cmd:get(CmdId),

            Time = util:unixtime(),
            {ok, _AffectedRows, LogId} = tbl_log_cmd_run:add([{cmd_id, CmdId}, {time, Time}, {cnt_all, length(Ids)}]),

            Fun = fun
                      (FId0) ->
                          FId = util:to_integer(FId0),
                          DetailData = [{log_id, LogId}, {cmd_id, CmdId}, {host_id, FId}, {time, Time}, {result, ""}],
                          {ok, _FAffectedRows, LogDetailId} = tbl_log_cmd_run_detail:add(DetailData),
                          case cmd:client_run(FId, LogDetailId, Cmd) of
                              {ok} -> ok;
                              {false, Reason} ->
                                  tbl_log_cmd_run_detail:update(LogDetailId, [{result_time, 1}, {result, util:fbin("~p", [Reason])}])
                          end
                  end,
            lists:foreach(Fun, Ids),

            web:echo_success(LogId);
        _ ->
            {ok, Cmds} = tbl_cmd:all_not_del(),
            {dtl, [{cmds, Cmds}]}
    end.

%% 命令执行日志
cmd_run_log(?web_get, Req, _Opts) ->
    Data= cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"act">>, Data) of
        <<"del">> ->
            Id = util:to_integer(proplists:get_value(<<"id">>, Data)),
            tbl_log_cmd_run:delete(Id),
            tbl_log_cmd_run_detail:delete_by_log_id(Id),
            {error, "删除成功"};
        _ ->
            #{page := Page, datas := Datas0} = web_page:page_order(Req, log_cmd_run, "`id` DESC"),
            Fun = fun
                      (FData = #{cmd_id := FCmdId}) ->
                          {ok, #{name := CmdName}} = tbl_cmd:get(FCmdId),
                          FData#{cmd => CmdName}
                  end,
            Datas = lists:map(Fun, Datas0),
            {dtl, [{page, Page}, {datas, Datas}]}
    end.

%% 命令执行日志详情
cmd_run_log_detail(?web_get, Req, _Opts) ->
    Data    = cowboy_req:parse_qs(Req),

    LogId   = proplists:get_value(<<"log_id">>, Data, <<>>),
    CmdId   = proplists:get_value(<<"cmd_id">>, Data, <<>>),
    HostId  = proplists:get_value(<<"host_id">>, Data, <<>>),

    Wheres  = ?IF(LogId =:= <<>>, [], begin erlweb_tpl:assign(log_id, LogId), [{log_id, LogId}] end),
    Wheres2 = ?IF(CmdId =:= <<>>, Wheres, begin erlweb_tpl:assign(cmd_id, CmdId), [{cmd_id, CmdId} | Wheres] end),
    Wheres3 = ?IF(HostId =:= <<>>, Wheres2, begin erlweb_tpl:assign(host_id, HostId), [{host_id, HostId} | Wheres2] end),

    {ok, Cmds} = tbl_cmd:all(),
    {ok, Hosts} = tbl_host:all(),

    #{page := Page, datas := Datas0} = web_page:page(Req, log_cmd_run_detail, [], Wheres3, "`id` DESC", []),
    Fun = fun
              (FData = #{cmd_id := FCmdId, host_id := FHostId, result := FResult}) ->
                  {ok, #{name := CmdName}} = tbl_cmd:get(FCmdId),
                  {ok, #{name := HostName}} = tbl_host:get(FHostId),
                  FResult2 =
                      case FResult of
                          <<>> -> "";
                          _ -> re:replace(FResult, "\n", "<br />", [global, {return, list}])
                      end,
                  FData#{cmd => CmdName, host => HostName, result => FResult2}
          end,
    Datas = lists:map(Fun, Datas0),
    {dtl, [{cmds, Cmds}, {hosts, Hosts}, {page, Page}, {datas, Datas}]}.


%% 任务列表
cron(?web_get, Req, _Opts) ->
    Data= cowboy_req:parse_qs(Req),
    Id  = util:to_integer(proplists:get_value(<<"id">>, Data)),
    case proplists:get_value(<<"act">>, Data) of
        <<"edit">> ->
            case Id of
                0 -> erlweb_tpl:assign(title, "添加任务");
                _ ->
                    erlweb_tpl:assign(title, "编辑任务"),
                    {ok, Host} = tbl_cron:get(Id),
                    erlweb_tpl:assign(data, Host)
            end,
            {dtl_edit};
        <<"del">> ->
            %tbl_cron:delete(Id),
            tbl_cron:update(Id, [{is_del, ?true}]),

            Ids = cross_srv_online:get_ids(),
            Time = util:unixtime(),
            {ok, _AffectedRows, LogId} = tbl_log_cron_sync:add([{cron_id, Id}, {is_del, ?true}, {time, Time}, {cnt_all, length(Ids)}]),

            Fun = fun
                      (FId0) ->
                          FId = util:to_integer(FId0),
                          DetailData = [{log_id, LogId}, {cron_id, Id}, {host_id, FId}, {is_del, ?true}, {time, Time}, {state, ?adm_cron_sync_state_begin}, {reason, ""}],
                          {ok, _FAffectedRows, LogDetailId} = tbl_log_cron_sync_detail:add(DetailData),
                          case cron:client_sync(FId, Id, LogDetailId) of
                              {ok} -> ok;
                              {false, Reason} ->
                                  tbl_log_cron_sync_detail:update(LogDetailId, [{state, ?adm_cron_sync_state_failed}, {reason, util:fbin("~p", [Reason])}])
                          end
                  end,
            lists:foreach(Fun, Ids),

            {error, "删除成功"};
        _ ->
            #{page := Page, datas := Datas} = web_page:page_where(Req, cron, [{is_del, ?false}]),
            {dtl, [{page, Page}, {datas, Datas}]}
    end;
cron(?web_post, Req, _Opts) ->
    {ok, Data, _Req2} = cowboy_req:read_urlencoded_body(Req),
    Id      = proplists:get_value(<<"id">>, Data, <<>>),
    Name    = string:trim(proplists:get_value(<<"name">>, Data, <<>>)),
    Expr    = string:trim(proplists:get_value(<<"expr">>, Data, <<>>)),
    Cmd     = string:trim(proplists:get_value(<<"cmd">>, Data, <<>>)),
    ?IF(Name =:= <<>> orelse Expr =:= <<>> orelse Cmd =:= <<>>, ?web_failed("请输入正确的数据"), skip),

    ExprErlang =
        case cron_util:parse_expr(Expr) of
            {ok, ExprErlang0} -> util_type:term_to_bitstring(ExprErlang0);
            {false, Reason} -> ?INFO("Reason:~p", [Reason]),?web_failed(Reason)
        end,

    Bind    = [{name, Name}, {expr, Expr}, {expr_erlang, ExprErlang}, {cmd, Cmd}],
    case Id of
        <<>> ->
            Bind2 = [{time, util:unixtime()} | Bind],
            tbl_cron:add(Bind2);
        _ -> tbl_cron:update(Id, Bind)
    end,
    web:echo_success().

%% 任务同步
cron_sync(_Method, Req, _Opts) ->
    Data = cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"act">>, Data) of
        <<"data">> ->
            Page = #{data := Datas0} = web_page:page_table(Req, host),
            Fun = fun(FData = #{time := FTime}) -> FData#{time => util:to_binary(util:date_sep_YmdHis(FTime))} end,
            Datas = lists:map(Fun, Datas0),
            web:echo_json(Page#{data => Datas});
        <<"sync">> ->
            IsAll = util:to_integer(proplists:get_value(<<"is_all">>, Data, 0)),
            Ids =
                case IsAll of
                    ?true -> cross_srv_online:get_ids();
                    ?false ->
                        IdsTmp = util_cowboy:get_values("ids", Data),
                        ?IF(IdsTmp =:= [], ?web_failed("请选择主机"), skip),
                        IdsTmp
                end,

            CronId = proplists:get_value(<<"cron_id">>, Data, <<>>),
            ?IF(CronId =:= <<>>, ?web_failed("请选择要同步的任务"), skip),
            {ok, #{expr_erlang := ExprErlang0, cmd := Cmd}} = tbl_cron:get(CronId),
            {ok, ExprErlang} = util_type:bitstring_to_term(ExprErlang0),

            Time = util:unixtime(),
            {ok, _AffectedRows, LogId} = tbl_log_cron_sync:add([{cron_id, CronId}, {time, Time}, {cnt_all, length(Ids)}]),

            Fun = fun
                      (FId0) ->
                          FId = util:to_integer(FId0),
                          DetailData = [{log_id, LogId}, {cron_id, CronId}, {host_id, FId}, {time, Time}, {state, ?adm_cron_sync_state_begin}, {reason, ""}],
                          {ok, _FAffectedRows, LogDetailId} = tbl_log_cron_sync_detail:add(DetailData),
                          case cron:client_sync(FId, CronId, ExprErlang, Cmd, LogDetailId) of
                              {ok} -> ok;
                              {false, Reason} ->
                                  tbl_log_cron_sync_detail:update(LogDetailId, [{state, ?adm_cron_sync_state_failed}, {reason, util:fbin("~p", [Reason])}])
                          end
                  end,
            lists:foreach(Fun, Ids),

            web:echo_success(LogId);
        _ ->
            {ok, Crons} = tbl_cron:all_not_del(),
            {dtl, [{crons, Crons}]}
    end.

%% 任务同步日志
cron_sync_log(?web_get, Req, _Opts) ->
    Data= cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"act">>, Data) of
        <<"del">> ->
            Id = proplists:get_value(<<"id">>, Data),
            tbl_log_cron_sync:delete(Id),
            tbl_log_cron_sync_detail:delete_by_log_id(Id),
            {error, "删除成功"};
        _ ->
            #{page := Page, datas := Datas0} = web_page:page_order(Req, log_cron_sync, "`id` DESC"),
            Fun = fun
                      (FData = #{cron_id := FCronId}) ->
                          {ok, #{name := CronName}} = tbl_cron:get(FCronId),
                          FData#{cron => CronName}
                  end,
            Datas = lists:map(Fun, Datas0),
            {dtl, [{page, Page}, {datas, Datas}]}
    end.

%% 任务同步日志详情
cron_sync_log_detail(?web_get, Req, _Opts) ->
    Data = cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"act">>, Data) of
        <<"retry">> ->
            LogDetailId = proplists:get_value(<<"id">>, Data),
            {ok, #{cron_id := CronId, host_id := HostId, is_del := IsDel0}} = tbl_log_cron_sync_detail:get(LogDetailId),
            {ok, #{expr_erlang := ExprErlang0, cmd := Cmd}} = tbl_cron:get(CronId),
            {ok, ExprErlang} = util_type:bitstring_to_term(ExprErlang0),
            IsDel = ?IF(IsDel0 =:= ?true, true, false),

            case cron:client_sync(util:to_integer(HostId), CronId, ExprErlang, Cmd, LogDetailId, IsDel) of
                {ok} ->
                    tbl_log_cron_sync_detail:update(LogDetailId, [{state, ?adm_cron_sync_state_begin}]),
                    {error, "重新同步成功"};
                {false, Reason0} ->
                    Reason = util:fbin("~p", [Reason0]),
                    tbl_log_cron_sync_detail:update(LogDetailId, [{state, ?adm_cron_sync_state_failed}, {reason, Reason}]),
                    {error, Reason}
            end;
        _ ->
            LogId   = proplists:get_value(<<"log_id">>, Data, <<>>),
            CronId  = proplists:get_value(<<"cron_id">>, Data, <<>>),
            HostId  = proplists:get_value(<<"host_id">>, Data, <<>>),

            Wheres  = ?IF(LogId =:= <<>>, [], begin erlweb_tpl:assign(log_id, LogId), [{log_id, LogId}] end),
            Wheres2 = ?IF(CronId =:= <<>>, Wheres, begin erlweb_tpl:assign(cron_id, CronId), [{cron_id, CronId} | Wheres] end),
            Wheres3 = ?IF(HostId =:= <<>>, Wheres2, begin erlweb_tpl:assign(host_id, HostId), [{host_id, HostId} | Wheres2] end),

            {ok, Crons} = tbl_cron:all(),
            {ok, Hosts} = tbl_host:all(),

            #{page := Page, datas := Datas0} = web_page:page(Req, log_cron_sync_detail, [], Wheres3, "`id` DESC", []),
            Fun = fun
                      (FData = #{cron_id := FCronId, host_id := FHostId, state := FState, reason := FReason}) ->
                          {ok, #{name := CronName}} = tbl_cron:get(FCronId),
                          {ok, #{name := HostName}} = tbl_host:get(FHostId),
                          FReason2 =
                              case FReason of
                                  <<>> -> "";
                                  _ -> re:replace(FReason, "\n", "<br />", [global, {return, list}])
                              end,
                          IsFailed = FState =:= ?adm_cron_sync_state_failed,
                          FData#{cron => CronName, host => HostName, state_desc => maps:get(FState, ?adm_cron_sync_states), is_failed => IsFailed, reason => FReason2}
                  end,
            Datas = lists:map(Fun, Datas0),
            {dtl, [{crons, Crons}, {hosts, Hosts}, {page, Page}, {datas, Datas}]}
    end.

%% 任务执行日志
cron_run_log(?web_get, Req, _Opts) ->
    Data    = cowboy_req:parse_qs(Req),
    CronId  = proplists:get_value(<<"cron_id">>, Data, <<>>),
    HostId  = proplists:get_value(<<"host_id">>, Data, <<>>),

    Wheres  = [],
    Wheres2 = ?IF(CronId =:= <<>>, Wheres, begin erlweb_tpl:assign(cron_id, CronId), [{cron_id, CronId} | Wheres] end),
    Wheres3 = ?IF(HostId =:= <<>>, Wheres2, begin erlweb_tpl:assign(host_id, HostId), [{host_id, HostId} | Wheres2] end),

    {ok, Crons} = tbl_cron:all(),
    {ok, Hosts} = tbl_host:all(),

    #{page := Page, datas := Datas0} = web_page:page(Req, log_cron_run, [], Wheres3, "`id` DESC", []),
    Fun = fun
              (FData = #{cron_id := FCronId, host_id := FHostId, result := FResult}) ->
                  {ok, #{name := CronName}} = tbl_cron:get(FCronId),
                  {ok, #{name := HostName}} = tbl_host:get(FHostId),
                  FResult2 =
                      case FResult of
                          <<>> -> "";
                          _ -> re:replace(FResult, "\n", "<br />", [global, {return, list}])
                      end,
                  FData#{cron => CronName, host => HostName, result => FResult2}
          end,
    Datas = lists:map(Fun, Datas0),
    {dtl, [{crons, Crons}, {hosts, Hosts}, {page, Page}, {datas, Datas}]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

echarts_summary(HostId, Num) ->
    {ok, DatasLoad} = tbl_stat_load:get(HostId, Num),
    EcLoad = echarts_load(DatasLoad),

    {ok, DatasCpuUsed} = tbl_stat_cpu_used:get(HostId, Num),
    EcCpuUsed = echarts_cpu_used(DatasCpuUsed),

    {ok, DatasMem} = tbl_stat_mem:get(HostId, Num),
    EcMem = echarts_mem(DatasMem),

    [{ec_load, jsx:encode(EcLoad)}, {ec_cpu_used, jsx:encode(EcCpuUsed)}, {ec_mem, jsx:encode(EcMem)}].

echarts_load(Datas) ->
    Fun = fun
              ([HiKey, Load1, Load5, Load15], {XAxisAcc, Load1Acc, Load5Acc, Load15Acc}) ->
                  {[HiKey | XAxisAcc], [Load1 | Load1Acc], [Load5 | Load5Acc], [Load15 | Load15Acc]}
          end,
    {XAxis, Load1, Load5, Load15} = lists:foldl(Fun, {[], [], [], []}, Datas),
    #{x_axis => XAxis, load1_series => Load1, load5_series => Load5, load15_series => Load15}.

echarts_cpu_used(Datas) ->
    Fun = fun
              ([HiKey, Used], {XAxisAcc, UsedAcc}) ->
                  {[HiKey | XAxisAcc], [Used | UsedAcc]}
          end,
    {XAxis, Used} = lists:foldl(Fun, {[], []}, Datas),
    #{x_axis => XAxis, used_series => Used}.

echarts_mem(Datas) ->
    Fun = fun
              ([HiKey, MemTotal, MemUsed], {XAxisAcc, MemTotalAcc, MemUsedAcc}) ->
                  {[HiKey | XAxisAcc], [ceil(MemTotal / 1024) | MemTotalAcc], [ceil(MemUsed / 1024) | MemUsedAcc]}
          end,
    {XAxis, MemTotal, MemUsed} = lists:foldl(Fun, {[], [], []}, Datas),
    #{x_axis => XAxis, total_series => MemTotal, used_series => MemUsed}.
