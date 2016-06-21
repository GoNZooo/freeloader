-module(freeloader_downloader).

%% API exports
-export([start_link/2,
         fetch/2, fetch/1]).

%% Internal export
-export([init/2]).


%% Callbacks
-callback start_link(Url :: string() | binary(),
                     Opts :: map()) -> {ok, pid()}.

-callback init(Url :: string(),
               Opts :: map()) -> {ok, Options :: map()}.

%% ==== %%
%% API  %%
%% ==== %%

start_link(Mod, Args) ->
    {ok, proc_lib:spawn_link(freeloader_downloader, init, [Mod, Args])}.
    

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

%% ========= %%
%% Internals %%
%% ========= %%

init(Mod, Args) ->
    {ok, Options} = apply(Mod, init, Args),
    Url = get_url(Options),
    Type = get_type(Options),
    ParseFun = get_parse_fun(Options),
    Timeout = get_timeout(Options),
    HTTPOpts = get_http_options(Options),
    download(Url, Type, ParseFun, Timeout, HTTPOpts).

download(Url, Type, ParseFun, Timeout, HTTPOpts) ->
    {ok, {_, _, Result}} = httpc:request(get, {Url, []},
                                         HTTPOpts, [{body_format, Type}]),
    ParsedResult = ParseFun(Result),

    receive
        {fetch, From, Ref} ->
            From ! {ok, Ref, ParsedResult}
    after Timeout ->
            exit(timeout)
    end.

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

get_http_options(#{http_opts := HTTPOpts}) ->
    HTTPOpts;
get_http_options(_) ->
    [].
