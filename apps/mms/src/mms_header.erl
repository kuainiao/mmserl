%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_header).

%% API
-export([verify_upload/1, parse/1, parse_body/1, action/2]).

-include("mms.hrl").


parse(Req) ->
    FileSize = parse_filesize(Req),
    Range = parse_range(Req),
    Owner = parse_owner(Req),
    case lists:member(undefined, [FileSize, Range, Owner]) of
        true -> false;
        _ ->
            #mms_headers{
                uid = parse_uid(Req),
                filename = parse_filename(Req),
                filesize = FileSize,
                range = Range,
                owner = Owner
            }
    end.


parse_uid(Req) ->
    cowboy_req:header(<<"uid">>, Req, undefined).

parse_filename(Req) ->
    cowboy_req:header(<<"filename">>, Req, <<"">>).

parse_filesize(Req) ->
    try binary_to_integer(cowboy_req:header(<<"filesize">>, Req)) of
        FileSize -> FileSize
    catch
        _:_ -> undefined
    end.

parse_owner(Req) ->
    cowboy_req:header(<<"owner">>, Req, undefined).

parse_range(Req) ->
    Range = cowboy_req:header(<<"range">>, Req),
    case Range of
        undefined -> undefined;
        _ ->
            case cowboy_http:range(Range) of
                {<<"bytes">>, [{S, E}]} ->
                    #mms_range{start_bytes = S, end_bytes = E};
                _ -> undefined
            end
    end.

parse_body(Req) ->
    {ok, _, Req2} = cowboy_req:part(Req),
    {ok, Data, _} = cowboy_req:part_body(Req2),
    Data.

verify_upload(Req) ->
    case cowboy_req:header(<<"token">>, Req) of
        undefined -> false;
        Token ->
            case mms_redis:get(<<"upload_token:",Token/binary>>) of
                {ok, _} -> true;
                _ -> false
            end
    end.

action(#mms_range{start_bytes = S, end_bytes = E}, FileSize) ->
    case FileSize of
        E ->
            case S of
                0 -> upload;
                _ -> append_upload
            end;
        _ ->
            case S of
                0 -> new_file;
                _ -> append_file
            end
    end.