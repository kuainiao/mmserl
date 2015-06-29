%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-define(PUBLIC, <<"1">>).
-define(PRIVATE, <<"0">>).

-define(GET_ENV(Key), mms_lib:get_env(Key)).
-define(S3_CONFIG, mms_s3:config()).

-define(DEBUG(X), io:format("debug:~p~n", [X])).


%% ==================
%% mms records
%% ==================

-record(mms_range, {
    start_bytes :: integer(),
    end_bytes :: integer()
}).

-record(mms_headers, {
    uid :: binary(),
    filename :: binary(),
    filesize :: integer(),
    public :: binary(),
    range :: #mms_range{},
    owner :: binary(),
    token :: binary(),
    expiration :: integer()
}).

-record(mms_file, {
    filename :: binary(),
    owner :: binary(),
    uid :: binary(),
    public :: public | private,
    created_at :: integer()
}).



