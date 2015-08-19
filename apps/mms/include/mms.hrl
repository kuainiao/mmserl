%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

%% is private
-define(PUBLIC, <<"0">>).
-define(PRIVATE, <<"1">>).

-define(ENV(Key), mms_lib:get_env(Key)).
-define(S3_CONFIG, mms_s3:config()).
-define(MMS_SECRET, mms_s3:secret()).
-define(DEBUG(X), io:format("debug:~p~n", [X])).


%% ==================
%% mms records
%% ==================

-record(mms_range, {
    start_bytes :: integer(),
    end_bytes :: integer(),
    total_bytes :: integer()
}).

-record(mms_headers, {
    uid :: binary(),
    filename :: binary(),
    filesize :: integer(),
    private :: binary(),
    range :: #mms_range{},
    owner :: binary(),
    token :: binary(),
    expiration :: integer()
}).

-record(mms_file, {
    filename :: binary(),
    owner :: binary(),
    uid :: binary(),
    private :: binary(),
    created_at :: integer()
}).



