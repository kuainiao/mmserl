%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_mysql).

-export([start/0, save/2, save/3]).

-include("mms.hrl").

start() ->
    mysql:start_link(mms_conn,
        ?GET_ENV(mysql_host),
        ?GET_ENV(mysql_port),
        ?GET_ENV(mysql_usr),
        ?GET_ENV(mysql_pwd),
        ?GET_ENV(mysql_db),
        fun log/4,
        utf8
    ).

log(Module, Line, Level, FormatFun) ->
    case Level of
        error ->
            {Format, Arguments} = FormatFun(),
            io:format("~w:~b: " ++ Format ++ "~n", [Module, Line] ++ Arguments);
        _ -> o
    end.

-spec save(binary(), binary()) -> binary()| {error, _}.
save(FileName, Owner) ->
    R2 = mysql:transaction(
        mms_conn,
        fun() -> {data, T1} = mysql:fetch(<<"select uuid()">>),
            [[Uid]] = mysql:get_result_rows(T1),
            mysql:fetch(<<"insert into mms_file(uid,filename,owner,created_at) values('", Uid/binary, "','",
            FileName/binary, "', '", Owner/binary, "', now());">>),
            Uid
        end),
    case R2 of
        {atomic, Uid} -> Uid;
        Reason -> {error, Reason}
    end.

-spec save(binary(), binary(), binary()) -> binary()| {error, _}.
save(Uid, FileName, Owner) ->
    Query = <<"insert into mms_file(uid,filename,owner,created_at) values('", Uid/binary, "','",
    FileName/binary, "', '", Owner/binary, "', now());">>,
    case mysql:fetch(mms_conn, Query) of
        {updated, {mysql_result, [], [], 1, 0, [], 0, []}} ->
            Uid;
        Reason ->
            {error, Reason}
    end.