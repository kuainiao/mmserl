%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_file).


%% API
-export([append/2, read/1, write/2, append_data/2, delete/1]).

append(FileName, Append) ->
    file:write_file("files/" ++ FileName, Append, [append]).

read(FileName) ->
    file:read_file("files/" ++ FileName).

append_data(FileName, Content) ->
    Content1 = read("files/" ++ FileName),
    <<Content1/binary, Content/binary>>.

write(FileName, Content) ->
    file:write_file("files/" ++ FileName, Content, [write]).

delete(FileName) ->
    file:delete("files/" ++ FileName).