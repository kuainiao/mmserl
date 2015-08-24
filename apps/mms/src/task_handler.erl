%%%-------------------------------------------------------------------
%%% @doc task: remove expiration upload
%%%
%%%-------------------------------------------------------------------

-module(task_handler).

-export([init/2]).

-include("mms.hrl").

-define(Expire, stamp() - 30 * 24 * 60 * 60).

init(Req, Opts) ->
    run(),
    mms_response:ok(Req, Opts, <<"done.">>).

run() ->
    case mms_redis:all() of
        {ok, R} when is_list(R) ->
            lists:foreach(fun(X) ->
                case mms_redis:get(X) of
                    {ok, <<P:1/binary, $:, Val/binary>>} ->
                        case is_expired(Val) of
                            error ->
                                io:format("bad value: ~p:~p~n", [X, Val]);
                            true ->
                                remove(X, P);
                            _ ->
                                ok
                        end;
                    _ ->
                        ok
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

remove(<<"upload:", Uid/binary>> = Key, Private) ->
    case mms_redis:get(<<"upload_temp:", Uid/binary>>) of
        {ok, Str} ->
            Ranges = mms_lib:str_to_ranges(Str),
            mms_lib:clear_tempfile(Uid, Private, Ranges),
            mms_redis:remove(Key),
            mms_redis:remove(<<"upload_temp:", Uid/binary>>),
            io:format("removed: ~p~n", [Uid]);
        _ ->
            mms_redis:remove(Key)
    end;
remove(_, _) ->
    ok.

stamp() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.
