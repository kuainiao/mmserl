%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_s3).

-export([
    config/0,
    bucket/1,
    start/0,
    get/2,
    get_object/1,
    upload/3,
    secret/0,
    remove/1,
    multi_init/1,
    multi_upload/4,
    multi_abort/2
]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include("mms.hrl").


start() ->
    erlcloud:start().

-spec config() -> #aws_config{}.
config() ->
    #aws_config{
        access_key_id = ?ENV(s3_key),
        secret_access_key = ?ENV(s3_secret),
        s3_host = ?ENV(s3_host),
        timeout = ?ENV(s3_timeout)
    }.

-spec secret() -> binary() | undefined.
secret() ->
    ?ENV(upload_secret).

-spec bucket(binary()) -> binary() | undefined.
bucket(?AVATAR) ->
    ?ENV(s3_public_bucket);
bucket(?PROJECT) ->
    ?ENV(s3_public_bucket);
bucket(_) ->
    ?ENV(s3_bucket).

-spec get(binary(), integer()) -> iodata() | error.
get(#mms_file{uid = Uid, type = ?AVATAR}, _) ->
    Host = list_to_binary(?ENV(s3_host)),
    Bucket = list_to_binary(bucket(?AVATAR)),
    <<"http://", Bucket/binary, ".", Host/binary, "/", Uid/binary>>;
get(#mms_file{uid = Uid, type = Type}, Expire) ->
    try erlcloud_s3:make_link(Expire, bucket(Type), Uid, ?S3_CONFIG) of
        {_, H, P} -> H ++ P
    catch
        _:_ -> error
    end.

-spec get_object(#mms_file{}) -> binary() | error.
get_object(#mms_file{uid = Uid, type = Type}) ->
    try erlcloud_s3:get_object(bucket(Type), binary_to_list(Uid), ?S3_CONFIG) of
        R ->
            proplists:get_value(content, R, error)
    catch
        _:_Reason ->
            ?DEBUG(_Reason),
            error
    end.

-spec upload(#mms_file{}, binary(), binary()) -> ok | error.
upload(#mms_file{uid = Uid, type = Type}, Content, ContentType) ->
    try erlcloud_s3:put_object(bucket(Type), binary_to_list(Uid), Content, [],
        [{"content-type", binary_to_list(ContentType)}], ?S3_CONFIG) of
        _ -> ok
    catch
        _:_Reason ->
            ?DEBUG(_Reason),
            error
    end.

-spec remove(#mms_file{}) -> ok | error.
remove(#mms_file{uid = Uid, type = Type}) ->
    try erlcloud_s3:delete_object(bucket(Type), binary_to_list(Uid), ?S3_CONFIG) of
        _ -> ok
    catch
        _:_Reason ->
            error
    end.

multi_init(#mms_file{uid = Uid, type = Type}) ->
    try erlcloud_s3:start_multipart(bucket(Type), binary_to_list(Uid), [], [], ?S3_CONFIG) of
        {ok, {uploadId, UploadId}} ->
            {ok, UploadId};
        _Reason ->
            {error, _Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.

multi_upload(#mms_file{uid = Uid, type = Type}, UploadId, PartNumber, Content) ->
    try erlcloud_s3:upload_part(bucket(Type), binary_to_list(Uid),
        binary_to_list(UploadId), PartNumber, Content, [], ?S3_CONFIG) of
        {ok, [{etag, ETag}]} ->
            {ok, list_to_binary(trim_etag(ETag))};
        _Reason ->
            ?DEBUG(_Reason),
            {error, _Reason}
    catch
        _:Reason ->
            ?DEBUG(Reason),
            {error, Reason}
    end.


multi_abort(#mms_file{uid = Uid, type = Type}, UploadId) ->
    try
        erlcloud_s3:abort_multipart(bucket(Type), Uid, UploadId, [], [], ?S3_CONFIG) of
        R -> R
    catch
        _:Reason ->
            {error, Reason}
    end.

trim_etag(ETag) ->
    T = ETag -- "\"",
    T -- "\"".