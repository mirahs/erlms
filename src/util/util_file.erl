%% -*- coding: latin-1 -*-
-module(util_file).

-export([
    write_consult/2
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

write_consult(Filename, Terms) ->
    case file:open(Filename, [write]) of
        {ok, Fd} ->
            do_write_consult(Fd, Terms),
            file:close(Fd),
            ok;
        {error, enoent} ->
            {false, util:fbin("文件(~p)不存在", [Filename])};
        {error, Error} ->
            {false, util:fbin("文件(~p)错误(~p)", [Filename, Error])}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

do_write_consult(_Fd, []) -> ok;
do_write_consult(Fd, [Term | Terms]) ->
    io:format(Fd, "~p.~n", [Term]),
    do_write_consult(Fd, Terms).
