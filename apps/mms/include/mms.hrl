%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------


-record(mms_range, {
    start_bytes :: integer(),
    end_bytes :: integer()
}).

-record(mms_headers, {
    uid :: binary() | undefined,
    filename :: binary(),
    filesize :: integer(),
    range :: #mms_range{} | undefined,
    owner :: binary()
}).

-define(GET_ENV(Key), mms_lib:get_env(Key)).

-define(S3_CONFIG, mms_s3:config()).
-define(BUCKET, mms_s3:bucket()).

-define(DEBUG(X),io:format("debug:~p~n",[X])).