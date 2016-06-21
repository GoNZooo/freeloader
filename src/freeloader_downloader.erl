-module(freeloader_downloader).

-export([start_link/2, init/2]).

-export([download/4, fetch/2, fetch/1]).

-type optlist() :: [option()].

-type option() :: {url, Url :: string() | binary()} |
                  {type, Type :: string | binary} |
                  {parse_fun, F :: fun((string() | binary()) -> any())} |
                  {timeout, Timeout :: non_neg_integer()}.

-callback start_link(Url :: string() | binary(),
                     Opts :: optlist()) -> {ok, pid()}.

-callback init(Url :: string(),
               Opts :: optlist()) -> {ok, Options :: optlist()}.

start_link(Mod, Args) ->
    {ok, proc_lib:spawn_link(freeloader_downloader, init, [Mod, Args])}.
    

download(Url, Type, ParseFun, Timeout) ->
    {ok, {_, _, Result}} = httpc:request(get, {Url, []},
                                         [], [{body_format, Type}]),
    ParsedResult = ParseFun(Result),

    receive
        {fetch, From, Ref} ->
            From ! {ok, Ref, ParsedResult}
    after Timeout ->
            exit(timeout)
    end.

fetch(Pid) ->
    fetch(Pid, timer:seconds(5)).

fetch(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {fetch, self(), Ref},
    receive
        {ok, Ref, Result} ->
            Result
    after Timeout ->
            {error, timeout}
    end.

init(Mod, Args) ->
    {ok, Options} = apply(Mod, init, Args),
    true = proplists:is_defined(url, Options),
    Url = proplists:get_value(url, Options),
    Type = proplists:get_value(type, Options, string),
    ParseFun = proplists:get_value(parse_fun, Options,
                                   fun(Data) -> Data end),
    Timeout = proplists:get_value(timeout, Options, timer:seconds(15)),
    download(Url, Type, ParseFun, Timeout).
