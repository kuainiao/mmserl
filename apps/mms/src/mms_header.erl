%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_header).

%% API
-export([verify/1, parse/1, parse_body/1, sign/5]).

-include("mms.hrl").


%% ==================================
%% parse header
%% ==================================

parse(Req) ->
    FileId = parse_fileid(Req),
    FileSize = parse_filesize(Req),
    Owner = parse_owner(Req),
    Token = parse_token(Req),
    Expiration = parse_expiration(Req),
    case lists:member(undefined, [FileId, FileSize, Owner, Token, Expiration]) of
        true -> false;
        _ ->
            {IsMultipart, UploadId, PartNumber} = is_multipart(Req),
            #mms_headers{
                fileid = FileId,
                filename = parse_filename(Req),
                filesize = FileSize,
                type = parse_type(Req),
                owner = Owner,
                token = Token,
                expiration = Expiration,
                mimetype = parse_content_type(Req),
                multipart = IsMultipart,
                partNumber = PartNumber,
                uploadid = UploadId
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

is_multipart(Req) ->
    case {parse_uploadid(Req), parse_partnum(Req)} of
        {undefined, _} -> {<<"0">>, undefined, undefined};
        {_, undefined} -> {<<"0">>, undefined, undefined};
        {UploadId, Num} -> {<<"1">>, UploadId, Num}
    end.

parse_partnum(Req) ->
    try binary_to_integer(cowboy_req:header(<<"partnumber">>, Req)) of
        Num -> Num
    catch
        _:_ -> undefined
    end.

parse_uploadid(Req) ->
    case cowboy_req:header(<<"uploadid">>, Req) of
        <<>> -> undefined;
        R -> R
    end.

parse_type(Req) ->
    case cowboy_req:header(<<"type">>, Req, ?MESSAGE) of
        ?AVATAR -> ?AVATAR;
        ?PROJECT -> ?PROJECT;
        _ -> ?MESSAGE
    end.

parse_owner(Req) ->
    cowboy_req:header(<<"owner">>, Req, undefined).

parse_token(Req) ->
    cowboy_req:header(<<"token">>, Req, undefined).

parse_expiration(Req) ->
    E = cowboy_req:header(<<"expiration">>, Req, undefined),
    try binary_to_integer(E) of
        I -> I
    catch
        _:_ -> undefined
    end.

parse_content_type(Req) ->
    cowboy_req:header(<<"mimetype">>, Req, <<"application/octet-stream">>).

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
verify(#mms_headers{owner = Owner, token = Token, fileid = FileId,
    expiration = Expiration, type = Type, multipart = IsMultipart}) ->
    case generate_timestamp() =< Expiration of
        true ->
            case mms_redis:get(<<"upload:", FileId/binary>>) of
                {ok, <<IsMultipart:1/binary, _/binary>>} ->
                    E = integer_to_binary(Expiration),
                    S = list_to_binary(?MMS_SECRET),
                    Token =:= sign(Owner, FileId, E, S, Type);
                _ ->
                    false
            end;
        _ ->
            ?DEBUG(<<"expired">>),
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


