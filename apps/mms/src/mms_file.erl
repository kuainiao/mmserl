%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mms_file).


%% API
-export([append/2, read/1, write/2, append_data/2]).
generate_filename() ->
    ok.

append(FileName, AppendData) ->
    file:write_file("files/" ++ FileName, AppendData, [append]).

read(FileName) ->
    file:read_file("files/" ++ FileName).

append_data(FileName, Data) ->
    Data1 = read("files/" ++ FileName),
    <<Data1/binary, Data/binary>>.

write(FileName, Data) ->
    file:write_file("files/" ++ FileName, Data, [write]).