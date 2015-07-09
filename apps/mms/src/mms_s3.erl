%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_s3).

-export([config/0, bucket/1, start/0, get/2, get_object/1, upload/2, secret/0]).

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
bucket(?PUBLIC) ->
    ?ENV(s3_public_bucket);
bucket(_) ->
    ?ENV(s3_bucket).

-spec get(binary(), integer()) -> iodata() | error.
get(#mms_file{uid = Uid, private = ?PUBLIC}, _) ->
    Host = list_to_binary(?ENV(s3_host)),
    Bucket = list_to_binary(bucket(?PUBLIC)),
    <<"http://", Bucket/binary, ".", Host/binary, "/", Uid/binary>>;
get(#mms_file{uid = Uid, private = Private}, Expire) ->
    try erlcloud_s3:make_link(Expire, bucket(Private), Uid, ?S3_CONFIG) of
        {_, H, P} -> H ++ P
    catch
        _:_ -> error
    end.

-spec get_object(#mms_file{}) -> binary() | error.
get_object(#mms_file{uid = Uid, private = Private}) ->
    try erlcloud_s3:get_object(bucket(Private), binary_to_list(Uid), ?S3_CONFIG) of
        R ->
            proplists:get_value(content, R, error)
    catch
        _:_Reason ->
%%          ?DEBUG(_Reason),
            error
    end.

-spec upload(#mms_file{}, binary()) -> ok | error.
upload(#mms_file{uid = Uid, private = Private}, Content) ->
    try erlcloud_s3:put_object(bucket(Private), binary_to_list(Uid), Content, ?S3_CONFIG) of
        _ -> ok
    catch
        _:_Reason ->
            error
    end.

