%%% -*- coding: latin-1 -*-

%%%===================================================================
%%% 配置
%%%===================================================================

%% 目录
-define(DIR_ROOT,   "./").
-define(DIR_PRIV,   ?DIR_ROOT ++ "priv/").

%% MySQL 数据库
-define(db_admin,       db_admin). % 数据库池 ID
-define(mysql_username, "root").    % 账号
-define(mysql_password, "root").    % 密码
-define(mysql_database, "erlms").   % 数据库

-define(salt_conf,      adsIIEIdak4551).


%%%===================================================================
%%% 日志记录
%%%===================================================================

-ifdef(debug).
-define(DEBUG(Msg), logger:debug(Msg, [], ?MODULE, ?LINE)).             % 输出调试信息
-define(DEBUG(F, A),logger:debug(F, A, ?MODULE, ?LINE)).
-define(INFO(Msg),  catch logger:info(Msg, [], ?MODULE, ?LINE)).        % 输出普通信息
-define(INFO(F, A), catch logger:info(F, A, ?MODULE, ?LINE)).
-define(ERR(Msg),   catch logger:error(Msg, [], ?MODULE, ?LINE)).       % 输出错误信息
-define(ERR(F, A),  catch logger:error(F, A, ?MODULE, ?LINE)).
-else.
-define(DEBUG(Msg), ok).
-define(DEBUG(F, A),ok).
-define(INFO(Msg),  catch logger_file:info(Msg, [], ?MODULE, ?LINE)).   % 输出普通信息
-define(INFO(F, A), catch logger_file:info(F, A, ?MODULE, ?LINE)).
-define(ERR(Msg),   catch logger_file:error(Msg, [], ?MODULE, ?LINE)).  % 输出错误信息
-define(ERR(F, A),  catch logger_file:error(F, A, ?MODULE, ?LINE)).
-endif.


%%%===================================================================
%%% 数据类型与常量
%%%===================================================================

%% 数字型 bool 值
-define(true, 1).
-define(false, 0).


%%%===================================================================
%%% 函数封装
%%%===================================================================

%% 转二进制 简写
-define(B(D),   (util:to_binary(D))/binary).
-define(BS(D),  (util_type:term_to_bitstring(D))/binary).

-define(IF(B, T, F),case (B) of true -> (T); false -> (F) end).
