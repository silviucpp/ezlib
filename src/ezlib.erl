-module(ezlib).
-author("silviu.caragea").

-include("ezlib.hrl").

-export([new/1, new/2, process/2, metrics/1]).

-spec(new(Method :: integer()) ->
    {ok, SessionRef :: reference()} | badarg | {error, Reason :: binary()}).

new(Method) ->
    ezlib_nif:new_session(Method).

-spec(new(Method :: integer(), Opt :: list()) ->
    {ok, SessionRef :: reference()} | badarg | {error, Reason :: binary()}).

new(Method, Opt) ->
    ezlib_nif:new_session(Method, Opt).

-spec(process(SessionRef :: reference(), Buffer :: binary() | iolist()) ->
    binary() | iolist() | badarg | {error, Reason :: binary()}).

process(SessionRef, Buffer) ->
    ezlib_nif:process_buffer(SessionRef, Buffer).

-spec(metrics(SessionRef :: reference()) ->
    {ok, Data :: list()} | badarg).

metrics(SessionRef) ->
    ezlib_nif:get_stats(SessionRef).