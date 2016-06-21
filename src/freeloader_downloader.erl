-module(freeloader_downloader).

-export([start_link/2, init/2]).

-export([download/4, fetch/2, fetch/1]).

-callback start_link(Url :: string() | binary(),
                     Opts :: map()) -> {ok, pid()}.

-callback init(Url :: string(),
               Opts :: map()) -> {ok, Options :: map()}.

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
    Url = get_url(Options),
    Type = get_type(Options),
    ParseFun = get_parse_fun(Options),
    Timeout = get_timeout(Options),
    download(Url, Type, ParseFun, Timeout).

get_url(#{url := Url}) ->
    Url.

get_type(#{type := Type}) ->
    Type;
get_type(_) ->
    string.

get_parse_fun(#{parse_fun := ParseFun}) ->
    ParseFun;
get_parse_fun(_) ->
    fun(Data) -> Data end.

get_timeout(#{timeout := Timeout}) ->
    Timeout;
get_timeout(_) ->
    timer:seconds(15).
