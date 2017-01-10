-module(integrity_test_SUITE).
-author("silviu.caragea").

-include_lib("common_test/include/ct.hrl").
-include("ezlib.hrl").

-compile(export_all).

all() -> [
    {group, ezlib_group}
].

groups() -> [
    {ezlib_group, [sequence], [
        test_compression_decompression_binary,
        test_compression_decompression_string,
        test_compression_decompression_iolist,
        test_bad_owner_process,
        test_compression_ratio
    ]}
].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_compression_decompression_binary(_Config) ->

    StringBin = <<"this is a string compressed with zlib nif library">>,

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE),
    {ok, InflateRef} = ezlib:new(?Z_INFLATE),

    CompressedBin = ezlib:process(DeflateRef, StringBin),
    DecompressedBin = ezlib:process(InflateRef, CompressedBin),

    CompressedBin2 = ezlib:process(DeflateRef, StringBin),
    DecompressedBin2 = ezlib:process(InflateRef, CompressedBin2),

    DecompressedBin = StringBin,
    DecompressedBin2 = StringBin,
    true.


test_compression_decompression_string(_Config) ->

    String = "this is a string compressed with zlib nif library",

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE, [{use_iolist, true}]),
    {ok, InflateRef} = ezlib:new(?Z_INFLATE, [{use_iolist, true}]),

    CompressedBin = ezlib:process(DeflateRef, String),
    DecompressedBin = ezlib:process(InflateRef, CompressedBin),

    CompressedBin2 = ezlib:process(DeflateRef, String),
    DecompressedBin2 = ezlib:process(InflateRef, CompressedBin2),

    String = DecompressedBin,
    String = DecompressedBin2,
    true.

test_compression_decompression_iolist(_Config) ->

    IoList = [[60,63,120,109,108,32,118,101, [" xml:lang='","en","'"],62]],
    String = lists:flatten(IoList),

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE, [{use_iolist, true}]),
    {ok, InflateRef} = ezlib:new(?Z_INFLATE, [{use_iolist, true}]),

    CompressedBin = ezlib:process(DeflateRef, IoList),
    DecompressedBin = ezlib:process(InflateRef, CompressedBin),

    CompressedBin2 = ezlib:process(DeflateRef, IoList),
    DecompressedBin2 = ezlib:process(InflateRef, CompressedBin2),

    String = DecompressedBin,
    String = DecompressedBin2,
    ok.

test_bad_owner_process(_Config) ->
    ParentProcess = self(),

    FunCreate = fun() ->
        {ok, Dr} = ezlib:new(?Z_DEFLATE),
        ParentProcess ! {session, Dr}
    end,
    spawn(FunCreate),

    DeflateRef = receive
        {session, S} ->
            S
    end,
    {error, _Reason} = ezlib:process(DeflateRef, <<"this is a string compressed with zlib nif library">>),
    true.

test_compression_ratio(_Config) ->
    Options = [
        {compression_level, 6},
        {window_bits, 10},
        {memory_level, 1}
    ],

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE, Options),
    ezlib_process(DeflateRef, 10),
    {ok, Opt} = ezlib:metrics(DeflateRef),
    true = is_list(Opt).

ezlib_process(_Ref, 0) ->
    ok;

ezlib_process(Ref, Nr) ->
    ezlib:process(Ref,
        <<"<iq from='silviu@woow.com' to='silviu@woow.com/macf57b9bf7' id='r3qu3st_@ct1v1ty_taskid' type='result'>
            <query xmlns='jabber:iq:activity' start='1452386757868' size='20' unread='0'/>
           </iq>">>),
    ezlib_process(Ref, Nr - 1).

