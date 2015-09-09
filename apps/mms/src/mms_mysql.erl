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
get(FileId) ->
    case mysql:fetch(mms_conn, <<"select uid,filename,owner,private,UNIX_timestamp(created_at) from mms_file where id= '",
    FileId/binary, "';">>) of
        {selected, {mysql_result, [], [], [], [], []}} ->
            {error, not_exists};
        {selected, {mysql_result, Uid, FileName, Owner, Type, CreatedAt}} ->
            #mms_file{id = FileId, uid = Uid, filename = FileName, owner = Owner,
                type = Type, created_at = CreatedAt};
        Reason ->
            {error, Reason}
    end.

save(#mms_file{id = FileId, filename = FileName, uid = Uid, type = Type, owner = Owner}) ->
    Query = <<"insert into mms_file(id,uid,filename,owner,type,created_at) values('", FileId/binary, "','",
    Uid/binary, "','", FileName/binary, "', '", Owner/binary, "',", Type/binary, ", now());">>,
    case mysql:fetch(mms_conn, Query) of
        {updated, {mysql_result, [], [], 1, 0, [], 0, []}} ->
            FileId;
        Reason ->
            {error, Reason}
    end.