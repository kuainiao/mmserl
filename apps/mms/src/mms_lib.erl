%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_lib).
-author("yue").

-export([app_start/0, get_env/1, binary_join/2]).

app_start() ->
    mms_s3:start(),
    mms_mysql:start(),
    mms_redis:start().

-spec get_env(atom()) -> string() | integer() | undefined.
get_env(Key) ->
    case application:get_env(mms, Key) of
        {ok, Val} -> Val;
        _ -> undefined
    end.

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(fun(A, B) ->
        if
            bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
            true -> A
        end
    end, <<>>, List).