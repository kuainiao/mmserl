%%%-------------------------------------------------------------------
%%% @doc task: remove expiration upload
%%%
%%%-------------------------------------------------------------------

-module(task).

%% API
-export([run/0]).
-define(Expire, stamp() - 30 * 24 * 60 * 60).
run() ->
    case mms_redis:all() of
        {ok, R} when is_list(R) ->
            lists:foreach(fun(X) ->
                case mms_redis:get(X) of
                    {ok, Val} ->
                        case is_expired(Val) of
                            error ->
                                io:format("bad value: ~p:~p~n", [X, Val]);
                            true ->
                                remove(X);
                            _ ->
                                ok
                        end
                end
            end, R)
    end,
    io:format("done.~n~n").

is_expired(Val) ->
    try binary_to_integer(Val) of
        I ->
            I < ?Expire
    catch
        _:_ ->

            false
    end.

remove(<<"upload:", Uid>> = Key) ->
    case mms_s3:remove(Uid) of
        ok ->
            mms_redis:remove(Key),
            io:format("removed: ~p~n", [Uid]);
        _ ->
            io:format("failed: ~p~n", [Uid])
    end;

remove(_) ->
    ok.

stamp() ->
    {M, S, _} = erlang:now(),
    M * 1000 + S.


