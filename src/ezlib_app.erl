-module(ezlib_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ezlib_sup:start_link().

stop(_State) ->
    ok.