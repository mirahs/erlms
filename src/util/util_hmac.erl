%%% -*- coding: latin-1 -*-
-module(util_hmac).

-export([
    sha1/2
]).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% HMAC-SHA1 算法
-spec sha1(Secret :: string() | binary(), Body :: string() | binary()) -> string().
sha1(Secret, Body) ->
    <<Mac:160/integer>> = crypto:hmac(sha, Secret, Body),
    lists:flatten(io_lib:format("~40.16.0b", [Mac])).
