-module(benchmark).
-author("silviu.caragea").

-include("ezlib.hrl").

-define(STANZA,  <<"<iq from='silviu@woow.com' to='silviu@woow.com/macf57b9bf7' id='r3qu3st_@ct1v1ty_taskid' type='result'>
   <query xmlns='jabber:iq:activity' start='1452386757868' size='20' unread='0'/>
 </iq>">>).

-export([run/5]).

ezlib_process(_Ref, 0) ->
    ok;

ezlib_process(Ref, Nr) ->
    ezlib:process(Ref, ?STANZA),
    ezlib_process(Ref, Nr - 1).

erlang_process(_Ref, 0) ->
    ok;

erlang_process(Ref, Nr) ->
    zlib:deflate(Ref, ?STANZA, sync),
    erlang_process(Ref, Nr - 1).

finish(Nr, Time) ->
    TimeMs = Time/1000,
    io:format("Time to complete: ~p ms  ~p per second ~n ", [TimeMs, Nr/(TimeMs/1000) ]).

run(erlang, Nr, Level, Bits, Mem) ->
    Z=zlib:open(),
    ok = zlib:deflateInit(Z, Level, deflated, Bits, Mem, default),
    {Time, _} = timer:tc( fun() -> erlang_process(Z, Nr) end),
    finish(Nr, Time),
    zlib:close(Z);

run(ezlib, Nr, Level, Bits, Mem) ->
    Options =
    [
        {compression_level, Level},
        {window_bits, Bits},
        {memory_level, Mem}
    ],

    {ok, DeflateRef} = ezlib:new(?Z_DEFLATE, Options),
    {Time, _} = timer:tc( fun() -> ezlib_process(DeflateRef, Nr) end),
    finish(Nr, Time),
    ezlib:metrics(DeflateRef).
