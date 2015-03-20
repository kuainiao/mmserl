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
            filesize = FileSize,
            token = Token
        } ->
            case mms_header:verify_upload(Req) of
                false ->
                    mms_response:bad_request(Req, Opts, <<"invalid token">>);
                _ ->
                    {ok, Data, Req3} = mms_header:parse_body(Req),
                    case mms_header:action(Range, FileSize) of
                        upload ->
                            Uid2 = uuid:generate(),
                            case mms_mysql:save(Uid2, FileName, Owner) of
                                {error, _} ->
                                    mms_response:bad_request(Req3, Opts, <<"invalid uid">>);
                                _ ->
                                    case mms_s3:upload(Uid2, Data) of
                                        ok ->
                                            mms_redis:remove(Token),
                                            mms_response:ok(Req3, Opts, Uid2);
                                        error ->
                                            mms_response:bad_request(Req3, Opts, <<"upload error">>)
                                    end
                            end;
                        append_upload ->
                            case mms_redis:get(<<"filename:", Uid/binary>>) of
                                {ok, _} ->
                                    Data = mms_file:append_data(Uid, Data),
                                    case mms_s3:upload(Uid, Data) of
                                        ok ->
                                            mms_redis:remove(<<"filename:", Uid/binary>>),
                                            mms_redis:remove(Token),
                                            mms_response:ok(Req3, Opts, Uid);
                                        error ->
                                            mms_response:bad_request(Req3, Opts, <<"upload error">>)
                                    end;
                                _ ->
                                    mms_response:bad_request(Req3, Opts, <<"invalid uid">>)
                            end;
                        new_file ->
                            Uid2 = uuid:generate(),
                            case mms_redis:insert(<<"filename:", Uid2/binary>>, Uid2) of
                                ok ->
                                    mms_file:append("files/" ++ Uid2, Data),
                                    mms_response:ok(Req3, Opts, Uid2);
                                _ ->
                                    mms_response:bad_request(Req3, Opts, <<"failed">>)
                            end;
                        append_file ->
                            case mms_redis:get(<<"filename:", Uid/binary>>) of
                                ok ->
                                    mms_file:append("files/" ++ Uid, Data),
                                    mms_response:ok(Req3, Opts, Uid);
                                _ ->
                                    mms_response:bad_request(Req3, Opts, <<"invalid uid">>)
                            end
                    end
            end;
        _ ->
            mms_response:bad_request(Req, Opts, <<"bad request">>)
    end.

