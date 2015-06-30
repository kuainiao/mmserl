%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_mysql).

-export([start/0, get/1, save/1]).

-include("mms.hrl").

start() ->
    mysql:start_link(mms_conn,
        ?ENV(mysql_host),
        ?ENV(mysql_port),
        ?ENV(mysql_usr),
        ?ENV(mysql_pwd),
        ?ENV(mysql_db),
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


%% ==========================
%% api
%% ===========================

-spec get(binary()) -> #mms_file{} | {error, _}.
get(Uid) ->
    case mysql:fetch(mms_conn, <<"select filename,owner,private,UNIX_timestamp(created_at) from mms_file where uid= '",
    Uid/binary, "';">>) of
        {selected, {mysql_result, [], [], [], []}} ->
            {error, not_exists};
        {selected, {mysql_result, FileName, Owner, Private, CreatedAt}} ->
            #mms_file{uid = Uid, filename = FileName, owner = Owner,
                private = case Private of
                              <<0>> -> ?PUBLIC;
                              _ -> ?PRIVATE
                          end, created_at = CreatedAt};
        Reason ->
            {error, Reason}
    end.

-spec save(#mms_file{}) -> binary()| {error, _}.
save(#mms_file{filename = FileName, uid = undefined, private = Private, owner = Owner}) ->
    R2 = mysql:transaction(
        mms_conn,
        fun() ->
            {data, T1} = mysql:fetch(<<"select uuid()">>),
            [[Uid]] = mysql:get_result_rows(T1),
            mysql:fetch(<<"insert into mms_file(uid,filename,owner,private,created_at) values('", Uid/binary, "','",
            FileName/binary, "', '", Owner/binary, "',", Private/binary, ", now());">>),
            Uid
        end),
    case R2 of
        {atomic, Uid} -> Uid;
        Reason -> {error, Reason}
    end;
save(#mms_file{filename = FileName, uid = Uid, private = Private, owner = Owner}) ->
    Query = <<"insert into mms_file(uid,filename,owner,private,created_at) values('", Uid/binary, "','",
    FileName/binary, "', '", Owner/binary, "',", Private/binary, ", now());">>,
    case mysql:fetch(mms_conn, Query) of
        {updated, {mysql_result, [], [], 1, 0, [], 0, []}} ->
            Uid;
        Reason ->
            {error, Reason}
    end.