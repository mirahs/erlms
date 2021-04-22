%% -*- coding: latin-1 -*-
-module(controller_api_master).

-compile(nowarn_export_all).
-compile(export_all).

-include("common.hrl").

%% 调用模块列表
-define(master_modules, [game_master]).
%% 调用函数列表
-define(master_funs,    []).


run(_Method, Req, _Opts) ->
    Datas = cowboy_req:parse_qs(Req),
    Result =
        case proplists:get_value(<<"data">>, Datas) of
            undefined -> parse_result(?false, "no data arg");
            BinData ->
                case catch handle(BinData) of
                    ok -> parse_result(?true, []);
                    {ok} -> parse_result(?true, []);
                    {ok, Reply} -> parse_result(?true, Reply);
                    {false, Reason} -> parse_result(?false, Reason);
                    Other ->
                        ?ERR("master通讯:处理数据[~s]时发生异常 Other:~n~p", [BinData, Other]),
                        parse_result(?false, util:fbin(<<"执行方法出错 Other:~p">>, [Other]))
                end
        end,
    {json, Result}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

handle(BinData) ->
    {ok, {Mod, Fun, Args}} = util_type:bitstring_to_term(BinData),
    %?DEBUG("Mod: ~w Fun: ~w, Args: ~w", [Mod, Fun, Args]),
    case auth_module(Mod, Fun) of
        true -> erlang:apply(Mod, Fun, Args);
        false ->
            ?ERR("Mod:~w,Fun:~w,Args:~w", [Mod, Fun, Args]),
            erlang:throw(auth_module_failed)
    end.

parse_result(?true, Data) ->
    jsx:encode([{code, ?true}, {data, Data}]);
parse_result(?false, Msg) ->
    jsx:encode([{code, ?false}, {msg, util:to_binary(Msg)}]).


auth_module(Module, Func) ->
    case lists:member(Module, ?master_modules) of
        true -> true;
        false -> lists:member({Module, Func}, ?master_funs)
    end.
