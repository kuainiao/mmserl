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
            uid = Uid,
            owner = Owner,
            range = Range,
            private = Private
        } = Headers ->
            case mms_header:verify(Headers) of
                false ->
                    make_response(Req, Opts, #mms_response{
                        uid = Uid,
                        status = 0,
                        ranges = [],
                        code = 10001
                    });
                _ ->
                    {ok, Content, Req3} = mms_header:parse_body(Req),
                    File = #mms_file{uid = Uid, filename = FileName, owner = Owner, private = Private},
                    case mms_lib:get_range(Uid, Range, FileSize) of
                        {complete, Ranges} ->
                            CompleteContent = mms_lib:get_complete_content(Ranges, Content, Range, Uid, Private),
                            case mms_s3:upload(File, CompleteContent) of
                                ok ->
                                    mms_redis:remove(<<"upload:", Uid/binary>>),
                                    mms_redis:remove(<<"upload_temp:", Uid/binary>>),
                                    case mms_mysql:save(File) of
                                        {error, _} ->
                                            make_response(Req3, Opts, #mms_response{
                                                uid = Uid,
                                                status = 0,
                                                ranges = Ranges,
                                                code = 10002
                                            });
                                        _ ->
                                            mms_lib:clear_tempfile(Uid, Private, Ranges),
                                            make_response(Req3, Opts, #mms_response{
                                                uid = Uid,
                                                status = 2,
                                                ranges = Ranges,
                                                code = 10000
                                            })
                                    end;
                                error ->
                                    make_response(Req3, Opts, #mms_response{
                                        uid = Uid,
                                        status = 0,
                                        ranges = Ranges,
                                        code = 10003
                                    })
                            end;
                        {append, Ranges} ->
                            SBin = integer_to_binary(Range#mms_range.start_bytes),
                            EBin = integer_to_binary(Range#mms_range.end_bytes),
                            case mms_s3:upload(#mms_file{
                                uid = <<Uid/binary, "-", SBin/binary, "-", EBin/binary>>
                                , private = Private}, Content) of
                                ok ->
                                    mms_redis:insert(<<"upload_temp:", Uid/binary>>, mms_lib:ranges_to_str(Ranges)),
                                    make_response(Req3, Opts, #mms_response{
                                        uid = Uid,
                                        status = 1,
                                        ranges = Ranges,
                                        code = 10000
                                    });
                                error ->
                                    make_response(Req3, Opts, #mms_response{
                                        uid = Uid,
                                        status = 0,
                                        ranges = Ranges,
                                        code = 10004
                                    })
                            end;
                        {error, Ranges} ->
                            make_response(Req3, Opts, #mms_response{
                                uid = Uid,
                                status = 0,
                                ranges = Ranges,
                                code = 10005
                            })
                    end
            end
    end.


%% ===========================
%% helper
%% ===========================

make_response(Req, Opts, Response) ->
    Resp = response_to_json(Response),
    Code = case Response#mms_response.status of
               1 -> 200;
               2 -> 200;
               _ -> 400
           end,
    mms_response:response(Req, Opts, Resp, Code, <<"application/json">>).

response_to_json(#mms_response{uid = Uid, status = Status, ranges = Ranges, code = Err}) ->
    S = integer_to_binary(Status),
    R = mms_lib:ranges_to_str(Ranges),
    E = integer_to_binary(Err),
    <<"{\"uid\":\"", Uid/binary, "\",\"status\":", S/binary, ",\"ranges\":\"", R/binary, "\",\"code\":", E/binary, "}">>.