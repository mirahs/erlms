%%% -*- coding: latin-1 -*-
-module(web_page).

-export([
    page/2
    ,page_field/3
    ,page_where/3
    ,page_order/3
    ,page_group/4
    ,page/6

    ,page_table/2
    ,page_table/4
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

page(Req, Table) ->
    page(Req, Table, [], [], [], []).
page_field(Req, Table, Fields) ->
    page(Req, Table, Fields, [], [], []).
page_where(Req,  Table, Wheres) ->
    page(Req, Table, [], Wheres, [], []).
page_order(Req, Table, Orders) ->
    page(Req, Table, [], [], Orders, []).
page_group(Req, Table, Orders, Groups) ->
    page(Req, Table, [], [], Orders, Groups).
page(Req, Table, Fields, Wheres, Orders, Groups) ->
    Data    = cowboy_req:parse_qs(Req),
    Page0   = proplists:get_value(<<"page">>, Data, <<>>),
    Limit0  = proplists:get_value(<<"limit">>, Data, <<>>),

    Page    = ?IF(Page0 =:= <<>>, 1, util:to_integer(Page0)),
    Limit   = ?IF(Limit0 =:= <<>>, 10, util:to_integer(Limit0)),
    Start   = (Page - 1) * Limit,

    Count   = get_count(Table, Fields, Wheres),

    Sql     = "SELECT " ++ format_field(Fields) ++ " FROM `" ++ util:to_list(Table) ++ "`" ++ format_where(Wheres) ++ format_group(Groups) ++ format_order(Orders) ++ " LIMIT " ++ util:to_list(Start) ++ "," ++ util:to_list(Limit),

    {ok, Datas} = mysql:select_map(?db_admin, Sql),
    #{page => #{curr => Page, limit => Limit, count => Count, query => page_query(Data)}, datas => Datas}.

page_table(Req, Table) ->
    page_table(Req, Table, [], []).
page_table(Req, Table, Fields, Wheres) ->
    Data    = cowboy_req:parse_qs(Req),
    Page0   = proplists:get_value(<<"page">>, Data, <<>>),
    Limit0  = proplists:get_value(<<"limit">>, Data, <<>>),

    Page    = ?IF(Page0 =:= <<>>, 1, util:to_integer(Page0)),
    Limit   = ?IF(Limit0 =:= <<>>, 10, util:to_integer(Limit0)),
    Start   = (Page - 1) * Limit,

    Count   = get_count(Table, Fields, Wheres),

    Sql     = "SELECT " ++ format_field(Fields) ++ " FROM `" ++ util:to_list(Table) ++ "`" ++ format_where(Wheres) ++ " LIMIT " ++ util:to_list(Start) ++ "," ++ util:to_list(Limit),

    {ok, Datas} = mysql:select_map(?db_admin, Sql),
    #{code => 0, msg => "", count => Count, data => Datas}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_count(Table, Fields, Wheres) ->
    {ok, [Count]} =
        case get_distinct(Fields) of
            {ok, Field} -> mysql:select_row(?db_admin, "SELECT count(DISTINCT `" ++ util:to_list(Field) ++ "`) AS `cc` FROM `" ++ util:to_list(Table) ++ "` " ++ format_where(Wheres));
            false -> mysql:select_row(?db_admin, "SELECT count(1) AS `cc` FROM `" ++ util:to_list(Table) ++ "` " ++ format_where(Wheres))
        end,
    Count.

get_distinct([]) -> false;
get_distinct(Fields) when is_list(Fields) ->
    get_distinct2(Fields);
get_distinct(_Fields) -> false.

get_distinct2([]) -> false;
get_distinct2([{_Distinct, Field} | _Fields]) -> {ok, Field};
get_distinct2([_Field | Fields]) ->
    get_distinct2(Fields).

page_query(Data0) ->
    Data2   = lists:keydelete(<<"page">>, 1, Data0),
    Data    = lists:keydelete(<<"limit">>, 1, Data2),
    Fun     = fun({K, V}, Acc) ->[util:to_list(K) ++ "=" ++ util:to_list(V) | Acc] end,
    Querys  = lists:foldl(Fun, [], Data),
    "&" ++ util:list_to_string(Querys, "", "&", "").


format_field([]) -> "*";
format_field(Fields) ->
    util:list_to_string(Fields, "`", "`,`", "`").

format_where(Wheres) ->
    {FieldList, DataList} = lists:unzip(Wheres),
    FieldString = util:list_to_string(FieldList, "`", "`=~s AND `", "`=~s"),
    case mysql:format(FieldString, DataList) of
        "" -> "";
        WhereStr -> " WHERE " ++ WhereStr
    end.

format_order([]) -> "";
format_order(Orders = [Item | _]) when is_tuple(Item); is_atom(Item) ->
    Fun = fun
              ({Field, Order}, OrderAcc) -> ["`" ++ util:to_list(Field) ++ "` " ++ util:to_list(Order) | OrderAcc];
              (Field, OrderAcc) -> ["`" ++ util:to_list(Field) ++ "`" | OrderAcc]
          end,
    Orders2 = lists:reverse(lists:foldl(Fun, [], Orders)),
    "ORDER BY " ++ util:list_to_string(Orders2, "", ",", "");
format_order(Order) ->
    "ORDER BY " ++ util:to_list(Order).

format_group([]) -> "";
format_group(Groups) when is_list(Groups) ->
    "GROUP BY " ++ util:list_to_string(Groups, "`", "`,`", "`");
format_group(Group) ->
    "GROUP BY `" ++ util:to_list(Group) ++ "`".
