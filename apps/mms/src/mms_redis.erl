%%%-------------------------------------------------------------------
%%% @doc Redis
%%%

%%%-------------------------------------------------------------------

-module(mms_redis).

-export([start/0, get/1, insert/2, remove/1]).

start() ->
    {ok, Client} = eredis:start_link(),
    register(redis_client, Client).

-spec get(binary()) -> binary() | {error, _}.
get(Key) ->
    case eredis:q(redis_client, ["GET", Key]) of
        {ok, undefined} ->
            {error, not_found};
        {ok, Result} ->
            Result;
        Error ->
            {error, Error}
    end.

-spec insert(binary(), binary()) -> ok | {error, _}.
insert(Key, Val) ->
    case eredis:q(redis_client, ["SET", Key, Val]) of
        {ok, <<"OK">>} ->
            ok;
        Error ->
            {error, Error}
    end.

remove(Key) ->
    case eredis:q(redis_client, ["DEL", Key]) of
        {ok, <<"OK">>} ->
            ok;
        Error ->
            {error, Error}
    end.
