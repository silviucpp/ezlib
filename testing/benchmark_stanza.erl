-module(benchmark_stanza).
-author("silviu.caragea").

-include("ezlib.hrl").

-export([run/6]).

ezlib_process(_Ref, _Lines, 0) ->
    ok;

ezlib_process(Ref, Lines, Nr) ->
    lists:foreach(fun(L) ->
    	ezlib:process(Ref, L) end, Lines),
    ezlib_process(Ref, Lines, Nr - 1).

erlang_process(_Ref, _Lines, 0) ->
    ok;

erlang_process(Ref, Lines, Nr) ->
    lists:foreach(fun(L) ->
    	zlib:deflate(Ref, L, sync) end, Lines),
    erlang_process(Ref, Lines, Nr - 1).

finish(Nr, Time) ->
    TimeMs = Time/1000,
    io:format("Time to complete: ~p ms  ~p per second ~n ", [TimeMs, Nr/(TimeMs/1000) ]).

run(erlang, Path, Nr, Level, Bits, Mem) ->
    Z=zlib:open(),
    ok = zlib:deflateInit(Z, Level, deflated, Bits, Mem, default),
    Lines = read_from_file(Path),
    {Time, _} = timer:tc( fun() -> erlang_process(Z, Lines, Nr) end),
    finish(Nr*length(Lines), Time),
    zlib:close(Z);

run(ezlib, Path, Nr, Level, Bits, Mem) ->
    Lines = read_from_file(Path),
    Options =
    [
        {compression_level, Level},
        {window_bits, Bits},
        {memory_level, Mem}
    ],

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE, Options),
    {Time, _} = timer:tc( fun() -> ezlib_process(DeflateRef, Lines, Nr) end),
    finish(Nr*length(Lines), Time),
    ezlib:metrics(DeflateRef).

read_from_file(Path) ->
    {ok, Device} = file:open(Path, [read]),
    get_lines(Device).

binary_join([]) ->
    <<"<stream>">>;
binary_join([Part]) ->
    Part;
binary_join([Head|Tail]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary , Value/binary>> end, Head, Tail).

get_lines(Device) ->
    get_lines(Device, [], []).

get_lines(Device, Accum, AccumStanza) ->
    case io:get_line(Device, "") of
        eof  ->
            file:close(Device),
            case Accum of
                []->
                    lists:reverse(AccumStanza);
                _->
                    lists:reverse([binary_join(Accum) | AccumStanza])
            end;
        Line ->
            BinLine = list_to_binary(Line),
            case BinLine of
                <<"SEND ", _Rest/binary>> ->
                    %io:format(<<"build stanza1: ~p ~n">>, [Accum]),
                    get_lines(Device, [], [binary_join(Accum) | AccumStanza]);
                <<"RECV ", _Rest/binary>> ->
                    %io:format(<<"build stanza2: ~p ~n">>, [Accum]),
                    get_lines(Device, [], [binary_join(Accum) | AccumStanza]);
                _ ->
                    %io:format(<<"__ADD line: ~p ~n">>, [BinLine]),
                    get_lines(Device, [BinLine|Accum], AccumStanza)
            end
    end.
