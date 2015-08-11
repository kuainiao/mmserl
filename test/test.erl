-module(test).
-export([upload/0]).

-define(SERVER, "localhost").
-define(FILE_NAME, "test.dat").
-define(TOKEN, "4137c131d5d868048c7e0322c7cc673").
-define(OWNER, "4d3b9a253dfb4bf4ac316e08f76b08ff@localhost").
-define(DESC, "test.txt").
-define(UID, "ca6156c6ddb04b74a4d7ae3454dfd49f").
-define(EXPIRATION, "1400142400").
-define(PRIVATE, "1").

upload() ->
    {ok, BinContent} = file:read_file(?FILE_NAME),
    Content = binary_to_list(BinContent),
    Size = integer_to_binary(filelib:file_size(?FILE_NAME)),
    Range = string:concat("bytes=0-", Size),

    inets:start(),
    ssl:start(),

    URL = "http://" ++ ?SERVER ++ ":8080/upload",
    Header = [{"uid", ?UID},
        {"filesize", Size},
        {"private", ?PRIVATE},
        {"range", Range},
        {"filename", ?FILE_NAME},
        {"owner", ?OWNER},
        {"token", ?TOKEN},
        {"expiration", ?EXPIRATION}],
    Boundary = "--aft_file_upload",
    Body = format_body(Boundary, [], [{file001, ?DESC, Content}]),
    ContentType = string:concat("multipart/form-data;boundary=", Boundary),
    R = httpc:request(post, {URL, Header, ContentType, Body}, [], []),
    io:format("R=~p~n", [R]).


format_body(Boundary, Fields, Files) ->
    FieldParts = lists:map(fun({FieldName, FieldContent}) ->
        [lists:concat(["--", Boundary]),
            lists:concat(["Content-Disposition: form-data; name=\"", atom_to_list(FieldName), "\""]),
            "",
            FieldContent]
    end, Fields),
    FieldParts2 = lists:append(FieldParts),
    FileParts = lists:map(fun({FieldName, FileName, FileContent}) ->
        [lists:concat(["--", Boundary]),
            lists:concat(["Content-Disposition: format-data; name=\"", atom_to_list(FieldName), "\"; filename=\"", FileName, "\""]),
            lists:concat(["Content-Type: ", "application/octet-stream"]),
            "",
            FileContent]
    end, Files),
    FileParts2 = lists:append(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
    string:join(Parts, "\r\n").