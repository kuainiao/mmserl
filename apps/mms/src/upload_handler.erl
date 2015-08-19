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
            filesize = FileSize,
            uid = Uid,
            owner = Owner,
            range = Range,
            private = Private
        } = Headers ->
            case mms_header:verify(Headers) of
                false ->
                    mms_response:bad_request(Req, Opts, <<"bad request">>);
                _ ->
                    {ok, Content, Req3} = mms_header:parse_body(Req),
                    File = #mms_file{uid = Uid, filename = FileName, owner = Owner, private = Private},
                    case get_range(Uid, Range, FileSize) of
                        {complete, Ranges} ->
                            CompleteContent = get_complete_content(Ranges, Content, Range, Uid, Private),
                            case mms_s3:upload(File, CompleteContent) of
                                ok ->
                                    mms_redis:remove(<<"upload:", Uid/binary>>),
                                    mms_redis:remove(<<"upload_temp:", Uid/binary>>),
                                    case mms_mysql:save(File) of
                                        {error, _} ->
                                            mms_response:bad_request(Req3, Opts, <<"upload error">>);
                                        _ ->
                                            mms_response:ok(Req3, Opts, Uid)
                                    end;
                                error ->
                                    mms_response:bad_request(Req3, Opts, <<"upload error">>)
                            end;
                        {append, Ranges} ->
                            SBin = integer_to_binary(Range#mms_range.start_bytes),
                            EBin = integer_to_binary(Range#mms_range.end_bytes),
                            case mms_s3:upload(#mms_file{
                                uid = <<Uid/binary, "-", SBin/binary, "-", EBin/binary>>
                                , private = Private}, Content) of
                                ok ->
                                    mms_redis:insert(<<"upload_temp:", Uid/binary>>, ranges_to_str(Ranges)),
                                    mms_response:ok(Req3, Opts, Uid);
                                error ->
                                    mms_response:bad_request(Req3, Opts, <<"upload error">>)
                            end
                    end
            end
    end.


%% ===========================
%% helper
%% ===========================

-spec get_range(binary(), #mms_range{}, integer()) -> {complete, [#mms_range{}]} |
{append, [#mms_range{}]} | {error, term()}.
get_range(Uid, Range, FileSize) ->
    Key = <<"upload_temp:", Uid/binary>>,
    Ranges = case mms_redis:get(Key) of
                 {ok, R} ->
                     RangeStr = [binary:split(X, <<"-">>) || X <- binary:split(R, <<",">>)],
                     [Range | [#mms_range{start_bytes = binary_to_integer(S), end_bytes = binary_to_integer(E)} ||
                         [S, E] <- RangeStr]];
                 {error, _} ->
                     [Range]
             end,
    case check_done(Ranges, FileSize) of
        true ->
            {complete, Ranges};
        false ->
            case lists:member(Range, Ranges) of
                true ->
                    {error, duplicated};
                false ->
                    {append, Ranges}
            end
    end.

-spec ranges_to_str([#mms_range{}]) -> binary().
ranges_to_str(Ranges) ->
    RangeStr = [integer_to_list(S) ++ "-" ++ integer_to_list(E) ||
        #mms_range{start_bytes = S, end_bytes = E} <- Ranges],
    list_to_binary(string:join(RangeStr, ",")).

check_done(Ranges, FileSize) ->
    lists:sum([E - S + 1 || #mms_range{start_bytes = S, end_bytes = E} <- Ranges]) =:= FileSize.

get_complete_content(Ranges, Content, Range, Uid, Private) ->
    SRanges = lists:sort(fun(X, Y) ->
        X#mms_range.start_bytes =< Y#mms_range.start_bytes
    end, Ranges),
    Objects = lists:map(fun(X) ->
        case X =:= Range of
            true -> Content;
            false ->
                SBin = integer_to_binary(X#mms_range.start_bytes),
                EBin = integer_to_binary(X#mms_range.end_bytes),
                case mms_s3:get_object(#mms_file{
                    uid = <<Uid/binary, "-", SBin/binary, "-", EBin/binary>>,
                    private = Private
                }) of
                    error ->
                        undefined;
                    R ->
                        R
                end
        end
    end, SRanges),
    mms_lib:binary_join(Objects, <<>>).