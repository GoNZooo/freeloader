-module(freeloader_example).

-behavior(freeloader_downloader).

-export([start_link/1, start_link/2, init/2]).

start_link(Url) ->
    start_link(Url, []).

start_link(Url, Opts) ->
    freeloader_downloader:start_link(?MODULE, [Url, Opts]).

init(Url, Opts) ->
    {ok, [{url, Url}] ++ Opts}.
