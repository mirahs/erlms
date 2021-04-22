%% -*- coding: latin-1 -*-
-module(web_adm_menu).

-export([
    menu_init/0
    ,menu_check/3
    , menus/1
]).

-include("web.hrl").
-include("web_adm.hrl").
-include("web_adm_menu.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% 菜单数据初始化
menu_init() ->
    ets:insert(?ets_web_menu_data, [{UserType, do_menu_init(UserType)} || UserType <- maps:keys(?adm_user_types_desc)]).

%% 检查路由菜单权限
menu_check(Path, Code, CodeSub) ->
    case lists:member({Code, CodeSub}, ?adm_non_check_cv) of
        true -> true;
        false ->
            Type = web_adm:get_type(),
            case Type of
                0 -> false;
                _ ->
                    case ets:lookup(?ets_web_menu_check, {Path, Type}) of
                        [{{Path, Type}, Flag}] -> Flag;
                        [] ->
                            Flag = menu_check2(Type, Code, CodeSub),
                            ets:insert(?ets_web_menu_check, {{Path, Type}, Flag}),
                            Flag
                    end
            end
    end.

%% 根据账号类型得到原始菜单数据
menus(UserType) ->
    [{UserType, Menus}] = ets:lookup(?ets_web_menu_data, UserType),
    Menus.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 检查菜单权限
menu_check2(UserType, Code, CodeSub) ->
    Menus = menus(UserType),
    menu_check3(Code, CodeSub, Menus).

menu_check3(Code, CodeSub, [#{code := Code, data := Data} | _Menus]) ->
    Fun = fun
              (#{data := DataSub}) ->
                  Fun2 = fun(#{code := CodeSubXX}) -> CodeSub =:= CodeSubXX end,
                  lists:any(Fun2, DataSub);
              (#{code := CodeSubXX}) ->
                  CodeSub =:= CodeSubXX
          end,
    lists:any(Fun, Data);
menu_check3(Code, CodeSub, [_Menu | Menus]) ->
    menu_check3(Code, CodeSub, Menus);
menu_check3(_Code, _CodeSub, []) ->
    false.


%% 菜单数据初始化(根据账号类型)
do_menu_init(UserType) ->
    Fun = fun
              (#{code := Code, data := Data} = Menu, MenusAcc) ->
                  Fun2 = fun
                             (#{key := Keys} = MenuSub, MenuSubsAcc) ->
                                 case Keys =:= [] orelse lists:member(UserType, Keys)  of
                                     true -> MenuSubsAcc ++ [do_menu_init_item(Code, MenuSub)];
                                     false -> MenuSubsAcc
                                 end;
                             (#{data := DataSub} = MenuSub, MenuSubsAcc) ->
                                 Fun3 = fun
                                            (#{key := KeysSub} = MenuSubSub, MenuSubSubsAcc) ->
                                                case KeysSub =:= [] orelse lists:member(UserType, KeysSub) of
                                                    true -> MenuSubSubsAcc ++ [do_menu_init_item(Code, MenuSubSub)];
                                                    false -> MenuSubSubsAcc
                                                end
                                        end,

                                 case lists:foldl(Fun3, [], DataSub) of
                                     [] -> MenuSubsAcc;
                                     DataSub2 ->
                                         MenuSub2 = MenuSub#{data => DataSub2},
                                         MenuSubsAcc ++ [MenuSub2]
                                 end
                         end,

                  case lists:foldl(Fun2, [], Data) of
                      [] -> MenusAcc;
                      Data2 ->
                          Menu2 = Menu#{data => Data2},
                          MenusAcc ++ [Menu2]
                  end
          end,
    lists:foldl(Fun, [], ?web_menus).

do_menu_init_item(_Code, Item = #{url := Url}) ->
    Item#{url => lists:concat(["/", util:to_list(?web_app_adm), "/", Url])};
do_menu_init_item(Code, Item = #{code := CodeSub}) ->
    Item#{url => lists:concat(["/", util:to_list(?web_app_adm), "/", Code, "/", CodeSub])}.
