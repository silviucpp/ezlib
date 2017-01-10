-module(ezlib).
-author("silviu.caragea").

-include("ezlib.hrl").

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
    binary() | string() | {error, reason()}).

process(SessionRef, Buffer) ->
    ezlib_nif:process_buffer(SessionRef, Buffer).

-spec(metrics(ezlib_session()) ->
    {ok, list()} | {error, reason()}).

metrics(SessionRef) ->
    ezlib_nif:get_stats(SessionRef).