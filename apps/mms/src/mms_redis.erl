%%%-------------------------------------------------------------------
%%% @doc Redis
%%%

%%%-------------------------------------------------------------------

-module(mms_redis).

-export([start/0, get/1, insert/2, remove/1]).

start() ->
    {ok, Client} = eredis:start_link(),
    register(redis_client, Client).

get(Key) ->
    case eredis:q(redis_client, ["GET", Key]) of
        {ok, Result} ->
            Result;
        Error ->
            {error, Error}
    end.

insert(Key, Val) ->
    case eredis:q(redis_client, ["SET", Key, Val]) of
        {ok, <<"OK">>} ->
            ok;
        Error ->
            Error
    end.

remove(Key) ->
    case eredis:q(redis_client, ["DEL", Key]) of
        {ok, <<"OK">>} ->
            ok;
        Error ->
            Error
    end.
