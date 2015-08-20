%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------
-module(mms_response).

-export([response/5, bad_request/3, ok/3]).

response(Req, Opts, Body, Code, ContentType) ->
    Req2 = cowboy_req:reply(Code, [
        {<<"content-type">>, ContentType}
    ], Body, Req),
    {ok, Req2, Opts}.

bad_request(Req, Opts, Reason) ->
    response(Req, Opts, Reason, 400, <<"text/plain">>).

ok(Req, Opts, Content) ->
    response(Req, Opts, Content, 200, <<"text/plain">>).