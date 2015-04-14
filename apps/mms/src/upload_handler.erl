%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(upload_handler).

-export([init/2]).

-include("mms.hrl").

init(Req, Opts) ->
    case mms_header:parse(Req) of
        #mms_headers{
            filename = FileName,
            uid = Uid,
            owner = Owner,
            range = Range,
            filesize = FileSize
        } = Headers ->
            case mms_header:verify_upload(Headers) of
                false ->
                    mms_response:bad_request(Req, Opts, <<"bad request">>);
                _ ->
                    {ok, Data, Req3} = mms_header:parse_body(Req),
                    case mms_header:action(Range, FileSize) of
                        upload ->
                            upload(Req3, Opts, Uid, FileName, Owner, Data, false);
                        append_upload ->
                            Data2 = mms_file:append_data(Uid, Data),
                            upload(Req3, Opts, Uid, FileName, Owner, Data2, true);
                        new_file ->
                            append_file(Req3, Opts, Uid, Data);
                        append_file ->
                            append_file(Req3, Opts, Uid, Data)
                    end
            end;
        _ ->
            mms_response:bad_request(Req, Opts, <<"bad request">>)
    end.

upload(Req3, Opts, Uid, FileName, Owner, Data, DeleteFile) ->
    case mms_s3:upload(Uid, Data) of
        ok ->
            mms_redis:remove(<<"upload:", Uid/binary>>),
            case mms_mysql:save(Uid, FileName, Owner) of
                {error, _} ->
                    case DeleteFile of
                        true ->
                            mms_file:delete(Uid);
                        false ->
                            ok
                    end,
                    mms_response:bad_request(Req3, Opts, <<"upload error">>);
                _ ->
                    mms_response:ok(Req3, Opts, Uid)
            end;
        error ->
            mms_response:bad_request(Req3, Opts, <<"upload error">>)
    end.

append_file(Req3, Opts, Uid, Data) ->
    mms_file:append(Uid, Data),
    mms_response:ok(Req3, Opts, Uid).