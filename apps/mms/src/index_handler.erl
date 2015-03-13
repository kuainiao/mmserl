%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(index_handler).

-export([init/2]).

-include("mms.hrl").

init(Req, Opts) ->
    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Pong!">>, Req),
    {ok, Req2, Opts}.

