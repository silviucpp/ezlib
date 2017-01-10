-module(ezlib).
-author("silviu.caragea").

-include("ezlib.hrl").

%% Maximum bytes passed to the NIF handler at once
%% Current value is erlang:system_info(context_reductions) * 10
-define(MAX_BYTES_TO_NIF, 20000).

-export([new/1, new/2, process/2, metrics/1]).

-spec(new(compression_method()) ->
    {ok, ezlib_session()} | {error, reason()}).

new(Method) ->
    ezlib_nif:new_session(Method).

-spec(new(compression_method(), [ezlib_option()]) ->
    {ok, ezlib_session()} | {error, reason()}).

new(Method, Opt) ->
    ezlib_nif:new_session(Method, Opt).

-spec(process(ezlib_session(), binary() | iolist()) ->
    binary() | {error, reason()}).

process(SessionRef, Buffer) ->
    process_buffer(SessionRef, Buffer).

-spec(metrics(ezlib_session()) ->
    {ok, list()} | {error, reason()}).

metrics(SessionRef) ->
    ezlib_nif:get_stats(SessionRef).

process_buffer(SessionRef, Data) when is_binary(Data) ->
    process_buffer(SessionRef, Data, byte_size(Data), <<>>);
process_buffer(SessionRef, Data) ->
    process_buffer(SessionRef, iolist_to_binary(Data)).

process_buffer(SessionRef, NewData, Size, Buffer) ->
    case Size > ?MAX_BYTES_TO_NIF of
        true ->
            <<Chunk:?MAX_BYTES_TO_NIF/binary, Rest/binary>> = NewData,
            case ezlib_nif:process_buffer(SessionRef, Chunk) of
                {ok, ProcessedData} ->
                    process_buffer(SessionRef, Rest, Size - ?MAX_BYTES_TO_NIF, get_buffer(Buffer, ProcessedData));
                Error ->
                    Error
            end;
        _ ->
            case ezlib_nif:process_buffer(SessionRef, NewData) of
                {ok, ProcessedData} ->
                    get_buffer(Buffer, ProcessedData);
                Error ->
                    Error
            end
    end.

get_buffer(<<>>, NewData) ->
    NewData;
get_buffer(Data, NewData) ->
    <<Data/binary, NewData/binary>>.