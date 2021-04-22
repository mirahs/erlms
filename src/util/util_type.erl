%% -*- coding: latin-1 -*-
-module(util_type).

-export([
    term_to_string/1
    ,string_to_term/1
    ,term_to_bitstring/1
    ,bitstring_to_term/1
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% term 序列化, 转换为string格式
term_to_string(Term) -> bitstring_to_list(term_to_bitstring(Term)).

%% term 序列化, 转换为 bitstring 格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) -> list_to_bitstring(io_lib:format("~w", [Term])).

%% term 反序列化, string 转换为 term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} -> erl_parse:parse_term(Tokens);
        {error, Err, _} -> {error, Err};
        Err -> {error, Err}
    end.

%% term反序列化, bitstring 转换为 term
bitstring_to_term(undefined) -> {ok, undefined};
bitstring_to_term(BitString) -> string_to_term(binary_to_list(BitString)).
