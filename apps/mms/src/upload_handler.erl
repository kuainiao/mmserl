%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(upload_handler).

-export([init/2]).

-include("mms.hrl").


%% =========================
%% api
%% =========================

init(Req, Opts) ->
    case mms_header:parse(Req) of
        #mms_headers{
            filename = FileName,
            filesize = FileSize,
            fileid = FileId,
            owner = Owner,
            range = Range,
            type = Type,
            mimetype = ContentType
        } = Headers ->
            ?DEBUG(ContentType),
            case mms_header:verify(Headers) of
                false ->
                    make_response(Req, Opts, #mms_response{
                        fileid = FileId,
                        ranges = [],
                        code = 10011
                    });
                _ ->
                    {ok, Content, Req3} = mms_header:parse_body(Req),
                    File = #mms_file{id = FileId, filename = FileName, owner = Owner, type = Type},
                    case mms_lib:get_range(FileId, Range, FileSize) of
                        {complete, Ranges} ->
                            Uid = uuid:generate(),
                            CompleteContent = mms_lib:get_complete_content(Ranges, Content, Range, FileId, Type),
                            NewFile = File#mms_file{uid = Uid},
                            case mms_s3:upload(NewFile, CompleteContent, ContentType) of
                                ok ->
                                    mms_redis:remove(<<"upload:", FileId/binary>>),
                                    mms_redis:remove(<<"upload_temp:", FileId/binary>>),
                                    case mms_mysql:save(NewFile) of
                                        {error, _} ->
                                            make_response(Req3, Opts, #mms_response{
                                                fileid = FileId,
                                                ranges = Ranges,
                                                code = 10012
                                            });
                                        _ ->
                                            mms_lib:clear_tempfile(FileId, Type, Ranges),
                                            make_response(Req3, Opts, #mms_response{
                                                fileid = FileId,
                                                ranges = Ranges,
                                                code = 10001
                                            })
                                    end;
                                error ->
                                    make_response(Req3, Opts, #mms_response{
                                        fileid = FileId,
                                        ranges = Ranges,
                                        code = 10013
                                    })
                            end;
                        {append, Ranges} ->
                            SBin = integer_to_binary(Range#mms_range.start_bytes),
                            EBin = integer_to_binary(Range#mms_range.end_bytes),
                            case mms_s3:upload(#mms_file{
                                uid = <<FileId/binary, "-", SBin/binary, "-", EBin/binary>>
                                , type = Type}, Content, ContentType) of
                                ok ->
                                    mms_redis:insert(<<"upload_temp:", FileId/binary>>, mms_lib:ranges_to_str(Ranges)),
                                    make_response(Req3, Opts, #mms_response{
                                        fileid = FileId,
                                        ranges = Ranges,
                                        code = 10002
                                    });
                                error ->
                                    make_response(Req3, Opts, #mms_response{
                                        fileid = FileId,
                                        ranges = Ranges,
                                        code = 10013
                                    })
                            end;
                        {error, Ranges} ->
                            make_response(Req3, Opts, #mms_response{
                                fileid = FileId,
                                ranges = Ranges,
                                code = 10014
                            })
                    end
            end
    end.


%% ===========================
%% helper
%% ===========================

make_response(Req, Opts, Response) ->
    Resp = response_to_json(Response),
    Code = case Response#mms_response.code < 10010 of
               true -> 200;
               _ -> 400
           end,
    mms_response:response(Req, Opts, Resp, Code, <<"application/json">>).

response_to_json(#mms_response{fileid = FileId, ranges = Ranges, code = Err}) ->
    R = mms_lib:ranges_to_str(Ranges),
    E = integer_to_binary(Err),
    <<"{\"fileid\":\"", FileId/binary, "\",\"ranges\":\"", R/binary, "\",\"code\":", E/binary, "}">>.