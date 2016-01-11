-module(testing).
-author("silviu.caragea").

-include("ezlib.hrl").

-export([test_compression/0,
         test_compression_string/0,
         test_memory_usage/2,
         test_compression_ratio/4,
         test_compression_erlang/0]).

test_compression() ->

    StringBin = <<"this is a string compressed with zlib nif library">>,

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE),
    {ok, InflateRef} = ezlib:new(?Z_INFLATE),

    {ok, CompressedBin} = ezlib:process(DeflateRef, StringBin),
    {ok, DecompressedBin} = ezlib:process(InflateRef, CompressedBin),

    {ok, CompressedBin2} = ezlib:process(DeflateRef, StringBin),
    {ok, DecompressedBin2} = ezlib:process(InflateRef, CompressedBin2),

    DecompressedBin = StringBin,
    DecompressedBin2 = StringBin,
    ok.

test_compression_string() ->

    String = "this is a string compressed with zlib nif library",

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE, [{use_iolist, true}]),
    {ok, InflateRef} = ezlib:new(?Z_INFLATE, [{use_iolist, true}]),

    {ok, CompressedBin} = ezlib:process(DeflateRef, String),
    {ok, DecompressedBin} = ezlib:process(InflateRef, CompressedBin),

    {ok, CompressedBin2} = ezlib:process(DeflateRef, String),
    {ok, DecompressedBin2} = ezlib:process(InflateRef, CompressedBin2),

    String = DecompressedBin,
    String = DecompressedBin2,
    ok.


test_compression_erlang() ->

    StringBin = <<"this is a string that will be compressed using the ezlib nif library">>,

    Zd=zlib:open(),
    ok= zlib:deflateInit(Zd, 1, deflated, 8, 1, default),
    CompressedBin = zlib:deflate(Zd, StringBin, sync),
    ok = zlib:close(Zd),

    Zi = zlib:open(),
    ok = zlib:inflateInit(Zi),
    [DecompressedBin] = zlib:inflate(Zi, CompressedBin),
    ok = zlib:close(Zi),

    StringBin = DecompressedBin,
    ok.

ezlib_process(_Ref, 0) ->
    ok;

ezlib_process(Ref, Nr) ->
    ezlib:process(Ref, <<"<iq from='silviu@woow.com' to='silviu@woow.com/macf57b9bf7' id='r3qu3st_@ct1v1ty_taskid' type='result'>
   <query xmlns='jabber:iq:activity' start='1452386757868' size='20' unread='0'/>
 </iq>">>),
    ezlib_process(Ref, Nr - 1).

test_compression_ratio(Nr, Level, Bits, Mem) ->
    Options = [
        {compression_level, Level},
        {window_bits, Bits},
        {memory_level, Mem}
    ],

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE, Options),
    ezlib_process(DeflateRef, Nr),
    ezlib:metrics(DeflateRef).

test_memory_usage(NrSessionsInflate, NrSessionDeflate) ->
    ListInflate = lists:seq(1, NrSessionsInflate),
    ListDeflate = lists:seq(1, NrSessionDeflate),

    OptionsDeflate = [
        {compression_level, 1},
        {window_bits, 8},
        {memory_level, 2}
    ],

    OptionsInflate = [
        {window_bits, 15}
    ],

    Mem1 = erlang:memory(),

    io:format(<<"Memory usage before: ~p ~n">>, [Mem1]),

    InflateItems = [ezlib:new(?Z_INFLATE, OptionsInflate) || _X <- ListInflate],

    Mem2 = erlang:memory(),

    io:format(<<"Memory usage after inflate allocation of ~p items: ~p ~n">>, [NrSessionsInflate, Mem2]),

    DeflateItems = [ezlib:new(?Z_DEFLATE, OptionsDeflate) || _X <- ListDeflate],

    Mem3 = erlang:memory(),

    io:format(<<"Memory usage after deflate allocation of ~p items: ~p ~n">>, [NrSessionDeflate, Mem3]),

    timer:sleep(15000),

    if
        NrSessionsInflate > 0 ->
            [{ok, Hi} | _] = InflateItems,
            ezlib:metrics(Hi);
        true ->
            ok
    end,

    if
        NrSessionDeflate > 0 ->
            [{ok, Hd} | _] = DeflateItems,
            ezlib:metrics(Hd);
        true ->
            ok
    end,
    ok.