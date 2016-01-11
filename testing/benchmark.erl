-module(benchmark).
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
    finish(Nr, Time),
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
    finish(Nr, Time),
    ezlib:metrics(DeflateRef).

read_from_file(Path) ->
    {ok, Device} = file:open(Path, [read]),
    get_lines(Device).

get_lines(Device) ->
    get_lines(Device, []).

get_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_lines(Device, [Line|Accum])
    end.
