%% -*- coding: latin-1 -*-
-module(util_cmd).

-export([
    run/1
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

run(Cmd) ->
    Port = open_port({spawn, Cmd}, [stream, in, eof, hide, exit_status]),
    run_data(Port, []).


%%%===================================================================
%%% Internal functions
%%%===================================================================

run_data(Port, Datas) ->
    receive
        {Port, {data, Data}} ->
            run_data(Port, [Datas | Data]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    true
            end,
            receive
                {'EXIT', Port, _} ->
                    ok
            after 1 -> % force context switch
                ok
            end,
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, lists:flatten(Datas)}
    end.
