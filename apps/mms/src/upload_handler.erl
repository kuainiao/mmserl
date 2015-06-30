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
                            upload(Req3, Opts, File, Content, false);
                        append_upload ->
                            Content2 = mms_file:append_data(Uid, Content),
                            upload(Req3, Opts, File, Content2, true);
                        new_file ->
                            append_file(Req3, Opts, Uid, Content);
                        append_file ->
                            append_file(Req3, Opts, Uid, Content)
                    end
            end;
        _ ->
            mms_response:bad_request(Req, Opts, <<"bad request">>)
    end.


%% ===========================
%% helper
%% ===========================

upload(Req3, Opts, #mms_file{uid = Uid} = File, Content, DeleteFile) ->
    case mms_s3:upload(File, Content) of
        ok ->
            mms_redis:remove(<<"upload:", Uid/binary>>),
            case mms_mysql:save(File) of
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

append_file(Req3, Opts, Uid, Content) ->
    mms_file:append(Uid, Content),
    mms_response:ok(Req3, Opts, Uid).