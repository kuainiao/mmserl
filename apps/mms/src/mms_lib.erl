%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_lib).
-author("yue").

-export([app_start/0, get_env/1]).

app_start() ->
    mms_s3:start(),
    mms_mysql:start(),
    mms_redis:start().

get_env(Key) ->
    case application:get_env(mms, Key) of
        {ok, Val} -> Val;
        _ -> undefined
    end.