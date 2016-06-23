-module(freeloader_example).

-behavior(freeloader_downloader).

-export([start_link/1, start_link/2, init/1, fetch/1, fetch/2]).

-spec start_link(Url :: string()) -> {ok, pid()}.
start_link(Url) ->
    start_link(Url, []).

-spec start_link(Url :: string(), Opts :: freeloader_downloader:options())
                -> {ok, pid()}.
start_link(Url, Opts) ->
    freeloader_downloader:start_link(?MODULE, [Url, Opts]).

-spec fetch(Pid :: pid()) -> term() | {error, timeout}.
fetch(Pid) ->
    freeloader_downloader:fetch(Pid).

-spec fetch(Pid :: pid(), Timeout :: timeout()) -> term() | {error, timeout}.
fetch(Pid, Timeout) ->
    freeloader_downloader:fetch(Pid, Timeout).

-spec init(Args :: [string() | [proplists:property()]])
          -> {ok, [proplists:property()]}.
init([Url, Opts]) ->
    {ok, [{url, Url} | Opts]}.
