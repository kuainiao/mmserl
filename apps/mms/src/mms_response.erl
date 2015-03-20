%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------
-module(mms_response).

-export([bad_request/3, ok/3]).

bad_request(Req, Opts, Reason) ->
    Req2 = cowboy_req:reply(400, [
        {<<"content-type">>, <<"text/plain">>}
    ], Reason, Req),
    {ok, Req2, Opts}.

ok(Req, Opts, Text) ->
    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], Text, Req),
    {ok, Req2, Opts}.