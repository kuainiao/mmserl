%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

%% file type
-define(AVATAR, <<"1">>).
-define(MESSAGE, <<"2">>).
-define(PROJECT, <<"3">>).

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
    fileid :: binary(),
    filename :: binary(),
    filesize :: integer(),
    type :: binary(),
    range :: #mms_range{},
    owner :: binary(),
    token :: binary(),
    expiration :: integer(),
    mimetype ::binary()
}).

-record(mms_file, {
    id :: binary(),
    filename :: binary(),
    owner :: binary(),
    uid :: binary(),
    type :: binary(),
    created_at :: integer()
}).

-record(mms_response, {
    fileid :: binary(),
    ranges :: [#mms_range{}],
    code :: integer()
}).

