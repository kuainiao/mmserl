%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_s3).

-export([config/0, bucket/1, start/0, get/1, upload/2, upload_secret/0]).

-include("mms.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

start() ->
    erlcloud:start().

config() ->
    #aws_config{
        access_key_id = ?GET_ENV(s3_key),
        secret_access_key = ?GET_ENV(s3_secret),
        s3_host = ?GET_ENV(s3_host),
        timeout = ?GET_ENV(s3_timeout)
    }.

-spec bucket(binary()) -> binary().
bucket(?PUBLIC) ->
    ?GET_ENV(s3_public_bucket);
bucket(_) ->
    ?GET_ENV(s3_bucket).

upload_secret() ->
    ?GET_ENV(upload_secret).

-spec get(binary()) -> binary().
get(Uid) ->
    case mms_mysql:get(Uid) of
        {error, _} ->
            error;
        File ->
            erlcloud_s3:make_link(30 * 60, bucket(File#mms_file.public), binary_to_list(Uid), ?S3_CONFIG)
    end.

-spec upload(#mms_file{}, binary()) -> ok | error.
upload(File, Content) ->
    try erlcloud_s3:put_object(bucket(File#mms_file.public), binary_to_list(File#mms_file.uid), Content, ?S3_CONFIG) of
        _ -> ok
    catch
        _:_ -> error
    end.