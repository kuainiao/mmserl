%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_header).

%% API
-export([verify/1, parse/1, parse_body/1]).

-include("mms.hrl").


%% ==================================
%% parse header
%% ==================================

parse(Req) ->
    FileId = parse_fileid(Req),
    FileSize = parse_filesize(Req),
    Range = parse_range(Req),
    Owner = parse_owner(Req),
    Token = parse_token(Req),
    Expiration = parse_expiration(Req),
    case lists:member(undefined, [FileId, FileSize, Range, Owner, Token, Expiration]) of
        true -> false;
        _ ->
            #mms_headers{
                fileid = FileId,
                filename = parse_filename(Req),
                filesize = FileSize,
                type = parse_type(Req),
                range = Range,
                owner = Owner,
                token = Token,
                expiration = Expiration
            }
    end.

parse_fileid(Req) ->
    cowboy_req:header(<<"fileid">>, Req, undefined).

parse_filename(Req) ->
    cowboy_req:header(<<"filename">>, Req, <<"">>).

parse_filesize(Req) ->
    try binary_to_integer(cowboy_req:header(<<"filesize">>, Req)) of
        FileSize -> FileSize
    catch
        _:_ -> undefined
    end.

parse_type(Req) ->
    case cowboy_req:header(<<"type">>, Req, ?MESSAGE) of
        ?AVATAR -> ?AVATAR;
        ?PROJECT -> ?PROJECT;
        _ -> ?MESSAGE
    end.

parse_owner(Req) ->
    cowboy_req:header(<<"owner">>, Req, undefined).

parse_range(Req) ->
    Range = cowboy_req:header(<<"range">>, Req, undefined),
    case Range of
        undefined -> undefined;
        _ ->
            case range(Range) of
                undefined -> undefined;
                R -> R
            end
    end.

parse_token(Req) ->
    cowboy_req:header(<<"token">>, Req, undefined).

parse_expiration(Req) ->
    E = cowboy_req:header(<<"expiration">>, Req, undefined),
    try binary_to_integer(E) of
        I -> I
    catch
        _:_ -> undefined
    end.

parse_body(Req) ->
    {ok, _, Req2} = cowboy_req:part(Req),
    stream_uploaded_file(Req2, <<>>).

stream_uploaded_file(Req, Binary) ->
    case cowboy_req:part_body(Req, [{read_timeout, 50000}]) of
        {ok, FullBinary, Req2} ->
            {ok, FullBinary, Req2};
        {more, PartialBinary, Req2} ->
            stream_uploaded_file(Req2, <<Binary/binary, PartialBinary/binary>>)
    end.

%% ==================================
%% verify
%% ==================================

-spec verify(#mms_headers{}) -> true | false.
verify(#mms_headers{owner = Owner, token = Token, fileid = Uid, expiration = Expiration, type = Type,
    range = Range, filesize = FileSize}) ->
    case {generate_timestamp() =< Expiration, Range#mms_range.end_bytes + 1 =< FileSize} of
        {true, true} ->
            case mms_redis:get(<<"upload:", Uid/binary>>) of
                {ok, _} ->
                    E = integer_to_binary(Expiration),
                    S = list_to_binary(?MMS_SECRET),
                    Token =:= sign(Owner, Uid, E, S, Type);
                _ ->
                    false
            end;
        _ ->
            false
    end.

sign(Owner, Uid, Expir, Secret, Type) ->
    iolist_to_binary(md5(<<Owner/binary, Uid/binary, Expir/binary, Secret/binary, Type/binary>>)).

-spec md5(string() | binary()) -> string().
md5(Text) ->
    lists:flatten([io_lib:format("~.16b", [N]) || N <- binary_to_list(erlang:md5(Text))]).

generate_timestamp() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

%% special range parse, like "bytes=0-600"
range(<<"bytes=", Rest/binary>>) ->
    [S, E] = binary:split(Rest, <<"-">>),
    try {binary_to_integer(S), binary_to_integer(E)} of
        {SI, EI} ->
            #mms_range{start_bytes = SI, end_bytes = EI}
    catch
        _:_ ->
            undefined
    end;
range(_Range) ->
    undefined.


