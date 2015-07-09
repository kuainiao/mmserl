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
            uid = Uid,
            owner = Owner,
            range = Range,
            filesize = FileSize,
            private = Private
        } = Headers ->
            case mms_header:verify(Headers) of
                false ->
                    mms_response:bad_request(Req, Opts, <<"bad request">>);
                _ ->
                    {ok, Content, Req3} = mms_header:parse_body(Req),
                    File = #mms_file{uid = Uid, filename = FileName, owner = Owner, private = Private},
                    case mms_header:action(Range, FileSize) of
                        upload ->
                            upload(Req3, Opts, File, Content, true);
                        append_upload ->
                            case mms_s3:get_object(File) of
                                error ->
                                    mms_response:bad_request(Req, Opts, <<"error">>);
                                R ->
                                    Content2 = <<R/binary, Content/binary>>,
                                    upload(Req3, Opts, File, Content2, true)
                            end;
                        new_file ->
                            upload(Req3, Opts, File, Content, false);
                        append_file ->
                            upload(Req3, Opts, File, Content, false)
                    end
            end;
        _ ->
            mms_response:bad_request(Req, Opts, <<"bad request">>)
    end.


%% ===========================
%% helper
%% ===========================

upload(Req3, Opts, #mms_file{uid = Uid} = File, Content, Complete) ->
    case mms_s3:upload(File, Content) of
        ok ->
            case Complete of
                true ->
                    mms_redis:remove(<<"upload:", Uid/binary>>),
                    case mms_mysql:save(File) of
                        {error, _} ->
                            mms_response:bad_request(Req3, Opts, <<"upload error">>);
                        _ ->
                            mms_response:ok(Req3, Opts, Uid)
                    end;
                _ ->
                    mms_response:ok(Req3, Opts, Uid)
            end;
        error ->
            mms_response:bad_request(Req3, Opts, <<"upload error">>)
    end.

