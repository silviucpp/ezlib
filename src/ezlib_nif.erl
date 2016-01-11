-module(ezlib_nif).
-author("silviu.caragea").

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(load_nif/0).

-export([
    new_session/1,
    new_session/2,
    process_buffer/2,
    get_stats/1
]).

%% nif functions

load_nif() ->
    SoName = get_nif_library_path(),
    io:format(<<"Loading library: ~p ~n">>, [SoName]),
    ok = erlang:load_nif(SoName, 0).

get_nif_library_path() ->
    case code:priv_dir(ezlib) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                false ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

new_session(_Method) ->
    ?NOT_LOADED.

new_session(_Method, _Opt) ->
    ?NOT_LOADED.

process_buffer(_SessionRef, _Buffer) ->
    ?NOT_LOADED.

get_stats(_SessionRef) ->
    ?NOT_LOADED.
