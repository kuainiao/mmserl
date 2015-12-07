%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_mysql).

-export([
    start/0,
    save/1,
    get_multipart/2,
    save_multipart/3
]).

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


save(#mms_file{id = FileId, filename = FileName, uid = Uid, type = Type, owner = Owner}) ->
    Query = <<"insert into mms_file(id,uid,filename,owner,type,created_at) values('", FileId/binary, "','",
    Uid/binary, "','", FileName/binary, "', '", Owner/binary, "',", Type/binary, ", now());">>,
    case mysql:fetch(mms_conn, Query) of
        {updated, _} ->
            {ok, FileId};
        Reason ->
            ?DEBUG(Reason),
            {error, Reason}
    end.

get_multipart(FileId, UploadId) ->
    Query = <<"select uid from mms_multipart where fileid='",
    FileId/binary, "' and upload_id='", UploadId/binary, "';">>,
    case mysql:fetch(mms_conn, Query) of
        {data, {mysql_result, _, [], 0, 0, [], 0, []}} ->
            {error, not_exists};
        {data, {mysql_result, _, [[Uid]], 0, 0, [], 0, []}} ->
            {ok, Uid};
        Reason ->
            ?DEBUG(Reason),
            {error, Reason}
    end.

save_multipart(UploadId, PartNumber, Etag) ->
    N = integer_to_binary(PartNumber),
    Query = <<"insert into mms_multipart_records(upload_id,part_number,etag) values('",
    UploadId/binary, "',", N/binary, ",'", Etag/binary, "');">>,
    case mysql:fetch(mms_conn, Query) of
        {updated, _} ->
            ok;
        Reason ->
            ?DEBUG(Reason),
            {error, Reason}
    end.