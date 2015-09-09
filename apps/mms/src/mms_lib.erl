%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_lib).
-author("yue").

-export([
    app_start/0,
    get_env/1,
    binary_join/2,
    get_range/3,
    ranges_to_str/1,
    str_to_ranges/1,
    get_complete_content/5,
    clear_tempfile/3
]).

-include("mms.hrl").

app_start() ->
    mms_s3:start(),
    mms_mysql:start(),
    mms_redis:start().

-spec get_env(atom()) -> string() | integer() | undefined.
get_env(Key) ->
    case application:get_env(mms, Key) of
        {ok, Val} -> Val;
        _ -> undefined
    end.

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(fun(A, B) ->
        if
            bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
            true -> A
        end
    end, <<>>, List).


%% =====================
%% ranges
%% =====================

-spec get_range(binary(), #mms_range{}, integer()) -> {complete, [#mms_range{}]} |
{append, [#mms_range{}]} | {error, term()}.
get_range(FileId, Range, FileSize) ->
    Key = <<"upload_temp:", FileId/binary>>,
    Ranges = case mms_redis:get(Key) of
                 {ok, R} ->
                     str_to_ranges(R);
                 {error, _} ->
                     []
             end,
    case lists:member(Range, Ranges) of
        true ->
            {error, Ranges};
        false ->
            All = [Range | Ranges],
            case check_done(All, FileSize) of
                true ->
                    {complete, All};
                false ->
                    {append, All}
            end
    end.

-spec str_to_ranges(binary()) -> [#mms_range{}].
str_to_ranges(Str) ->
    RangeStr = [binary:split(X, <<"-">>) || X <- binary:split(Str, <<",">>)],
    [#mms_range{start_bytes = binary_to_integer(S), end_bytes = binary_to_integer(E)} ||
        [S, E] <- RangeStr].

-spec ranges_to_str([#mms_range{}]) -> binary().
ranges_to_str(Ranges) ->
    RangeStr = [range_to_str(R) || R <- Ranges],
    mms_lib:binary_join(RangeStr, <<",">>).

range_to_str(Range) ->
    SBin = integer_to_binary(Range#mms_range.start_bytes),
    EBin = integer_to_binary(Range#mms_range.end_bytes),
    <<SBin/binary, "-", EBin/binary>>.

check_done(Ranges, FileSize) ->
    lists:sum([E - S + 1 || #mms_range{start_bytes = S, end_bytes = E} <- Ranges]) =:= FileSize.

get_complete_content(Ranges, Content, Range, FileId, Type) ->
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
                    uid = <<FileId/binary, "-", SBin/binary, "-", EBin/binary>>,
                    type = Type
                }) of
                    error ->
                        undefined;
                    R ->
                        R
                end
        end
    end, SRanges),
    mms_lib:binary_join(Objects, <<>>).

clear_tempfile(FileId, Type, Ranges) ->
    lists:foreach(fun(X) ->
        SBin = integer_to_binary(X#mms_range.start_bytes),
        EBin = integer_to_binary(X#mms_range.end_bytes),
        Id = <<FileId/binary, "-", SBin/binary, "-", EBin/binary>>,
        mms_s3:remove(#mms_file{uid = Id, type = Type})
    end, Ranges).