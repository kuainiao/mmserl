%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(upload_handler).

-export([init/2, allowed_methods/2]).

-include("mms.hrl").


%% =========================
%% api
%% =========================

init(Req, Opts) ->
    ?DEBUG(Req),
    case mms_header:parse(Req) of
        #mms_headers{
            fileid = FileId,
            multipart = IsMultiPart
        } = Headers ->
            case mms_header:verify(Headers) of
                false ->
                    make_response(Req, Opts, #mms_response{
                        fileid = FileId,
                        code = 10010,
                        multipart = IsMultiPart
                    });
                _ ->
                    case IsMultiPart of
                        <<"0">> ->
                            upload_file(Req, Opts, Headers);
                        <<"1">> ->
                            upload_multi(Req, Opts, Headers)
                    end
            end;
        false ->
            make_response(Req, Opts, #mms_response{
                code = 10010
            })
    end.


allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.


%% ===========================
%% helper
%% ===========================

upload_file(Req, Opts, #mms_headers{
    filename = FileName,
    fileid = FileId,
    owner = Owner,
    type = Type,
    mimetype = ContentType,
    multipart = IsMultiPart
}) ->
    File = #mms_file{id = FileId, filename = FileName, owner = Owner, type = Type},
    Uid = uuid:generate(),
    case mms_header:parse_body(Req) of
        {ok, Content, Req3} ->
            case mms_s3:upload(File#mms_file{uid = Uid}, Content, ContentType) of
                ok ->
                    case mms_mysql:save(File#mms_file{uid = Uid}) of
                        {error, _} ->
                            make_response(Req3, Opts, #mms_response{
                                fileid = FileId,
                                code = 10012,
                                multipart = IsMultiPart
                            });
                        {ok, _} ->
                            mms_redis:remove(<<"upload:", FileId/binary>>),
                            make_response(Req3, Opts, #mms_response{
                                fileid = FileId,
                                code = 10001,
                                multipart = IsMultiPart
                            })
                    end;
                error ->
                    make_response(Req3, Opts, #mms_response{
                        fileid = FileId,
                        code = 10011,
                        multipart = IsMultiPart
                    })
            end;
        _Reason ->
            ?DEBUG(_Reason),
            make_response(Req, Opts, #mms_response{
                fileid = FileId,
                code = 10020,
                multipart = IsMultiPart
            })
    end.

upload_multi(Req, Opts, #mms_headers{
    filename = FileName,
    fileid = FileId,
    owner = Owner,
    type = Type,
    multipart = IsMultiPart,
    uploadid = UploadId,
    partNumber = PartNumber
}) ->
    File = #mms_file{id = FileId, filename = FileName, owner = Owner, type = Type},
    Response = #mms_response{
        fileid = FileId,
        multipart = IsMultiPart,
        uploadid = UploadId,
        partNumber = PartNumber
    },
    case mms_mysql:check_part(UploadId, PartNumber) of
        ok ->
            case mms_mysql:get_multipart(FileId, UploadId) of
                {ok, Uid} ->
                    case mms_header:parse_body(Req) of
                        {ok, Content, Req3} ->
                            case mms_s3:multi_upload(File#mms_file{uid = Uid},
                                UploadId, PartNumber, Content) of
                                {ok, Etag} ->
                                    case mms_mysql:save_multipart(UploadId, PartNumber, Etag) of
                                        ok ->
                                            make_response(Req3, Opts, Response#mms_response{
                                                code = 10002,
                                                etag = Etag
                                            });
                                        {error, _} ->
                                            make_response(Req3, Opts, Response#mms_response{
                                                code = 10013
                                            })
                                    end;
                                {error, _} ->
                                    make_response(Req3, Opts, Response#mms_response{
                                        code = 10014
                                    })
                            end;
                        _Reason ->
                            ?DEBUG(_Reason),
                            make_response(Req, Opts, #mms_response{
                                fileid = FileId,
                                code = 10020,
                                multipart = IsMultiPart
                            })
                    end;
                {error, _} ->
                    make_response(Req, Opts, Response#mms_response{
                        code = 10015
                    })
            end;
        _ ->
            make_response(Req, Opts, Response#mms_response{
                code = 10013
            })
    end.

make_response(Req, Opts, Response) ->
    Resp = response_to_json(Response),
    Code = case Response#mms_response.code < 10010 of
               true -> 200;
               _ -> 400
           end,
    mms_response:response(Req, Opts, Resp, Code, <<"application/json">>).

response_to_json(Response) ->
    [_ | L] = tuple_to_list(Response),
    Json = lists:filter(fun({_, Value}) ->
        case Value of
            undefined -> false;
            null -> false;
            _ -> true
        end
    end, lists:zip(record_info(fields, mms_response), L)),
    StrList = ["\"" ++ to_list(K) ++ "\":\"" ++ to_list(V) ++ "\"" || {K, V} <- Json],
    "{" ++ string:join(StrList, ",") ++ "}".

to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(_) ->
    "undefined".

