-module(benchmark).
-author("silviu.caragea").

-include("ezlib.hrl").

-export([run/7, run_stanza/7]).

run(Driver, Path, NrProcesses, Nr, Level, Bits, Mem) ->
    Lines = read_from_file(Path, lines),
    internal_run(Driver, Lines, NrProcesses, Nr, Level, Bits, Mem).

run_stanza(Driver, Path, NrProcesses, Nr, Level, Bits, Mem) ->
    Lines = read_from_file(Path, stanza),
    internal_run(Driver, Lines, NrProcesses, Nr, Level, Bits, Mem).

internal_run(Driver, LinesList, NrProcesses, Nr, Level, Bits, Mem) ->
    TotalBytes = lists:foldl(fun(X, Acc) -> Acc + byte_size(X) end, 0, LinesList),
    Fun = get_benchmark_fun(Driver, Nr, LinesList, Level, Bits, Mem),
    {Time, _} = timer:tc(fun() -> do_work(Fun, NrProcesses) end),
    finish(NrProcesses*Nr*TotalBytes, Time).

get_benchmark_fun(ezlib, Nr, LinesList, Level, Bits, Mem) ->
    fun(C) ->
        {ok, DeflateRef} = ezlib:new(?Z_DEFLATE, [
            {compression_level, Level},
            {window_bits, Bits},
            {memory_level, Mem}
        ]),
        ezlib_process(DeflateRef, LinesList, Nr),
        show_metrics(C, DeflateRef)
    end;
get_benchmark_fun(erlang, Nr, LinesList, Level, Bits, Mem) ->
    fun(_C) ->
        Z=zlib:open(),
        ok = zlib:deflateInit(Z, Level, deflated, Bits, Mem, default),
        erlang_process(Z, LinesList, Nr),
        zlib:close(Z)
    end.

show_metrics(1, Ref) ->
    spawn(fun() -> io:format("METRICS: ~p ~n", [ezlib:metrics(Ref)]) end);
show_metrics(_, _Ref) ->
    ok.

finish(TotalBytes, Time) ->
    TimeMs = Time/1000,
    BytesPerSecond = TotalBytes/(TimeMs/1000),
    io:format("Time to complete: ~p ms  ~s/s ~n", [TimeMs, format_size(BytesPerSecond)]).

ezlib_process(_Ref, _Lines, 0) ->
    ok;
ezlib_process(Ref, Lines, Nr) ->
    lists:foreach(fun(L) ->  ezlib:process(Ref, L) end, Lines),
    ezlib_process(Ref, Lines, Nr - 1).

erlang_process(_Ref, _Lines, 0) ->
    ok;
erlang_process(Ref, Lines, Nr) ->
    lists:foreach(fun(L) -> zlib:deflate(Ref, L, sync) end, Lines),
    erlang_process(Ref, Lines, Nr - 1).

read_from_file(Path, stanza) ->
    {ok, Device} = file:open(Path, [read]),
    get_stanza(Device);
read_from_file(Path, lines) ->
    {ok, Device} = file:open(Path, [read]),
    get_lines(Device).

get_stanza(Device) ->
    get_stanza(Device, [], []).

get_stanza(Device, Accum, AccumStanza) ->
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
                    get_stanza(Device, [], [binary_join(Accum) | AccumStanza]);
                <<"RECV ", _Rest/binary>> ->
                    %io:format(<<"build stanza2: ~p ~n">>, [Accum]),
                    get_stanza(Device, [], [binary_join(Accum) | AccumStanza]);
                _ ->
                    %io:format(<<"__ADD line: ~p ~n">>, [BinLine]),
                    get_stanza(Device, [BinLine|Accum], AccumStanza)
            end
    end.

get_lines(Device) ->
    get_lines(Device, []).

get_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  ->
            file:close(Device), Accum;
        Line ->
            get_lines(Device, [list_to_binary(Line)|Accum])
    end.

binary_join([]) ->
    <<"<stream>">>;
binary_join([Part]) ->
    Part;
binary_join([Head|Tail]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary , Value/binary>> end, Head, Tail).

do_work(Fun, Count) ->
    process_flag(trap_exit, true),
    spawn_childrens(Fun, Count),
    wait_responses(Count).

spawn_childrens(_Fun, 0) ->
    ok;
spawn_childrens(Fun, Count) ->
    spawn_link(fun() -> Fun(Count) end),
    spawn_childrens(Fun, Count -1).

wait_responses(0) ->
    ok;
wait_responses(Count) ->
    receive
        {'EXIT',_FromPid, Reason} ->
            case Reason of
                normal ->
                    ok;
                _ ->
                    io:format("process exit with error:~p", [Reason])
            end,
            wait_responses(Count -1)
    end.

format_size(Size) ->
    format_size(Size, ["B","KB","MB","GB","TB","PB"]).

format_size(S, [_|[_|_] = L]) when S >= 1024 -> format_size(S/1024, L);
format_size(S, [M|_]) ->
    io_lib:format("~.2f ~s", [float(S), M]).