-module(freeloader_downloader).

%% API exports
-export([start_link/2, fetch/2, fetch/1]).

%% Internal export
-export([init/2]).

%% Types
-export_type([url/0, type/0, parse_fun/0, options/0, http_options/0]).

-type options() :: [option()].

-type option() :: {url, url()} |
                  {type, type()} |
                  {parse_fun, parse_fun()} |
                  {timeout, timeout()} |
                  {http_options, http_options()}.

-type http_options() :: [proplists:property()].

-type downloader() :: atom() | pid().
-type url() :: string().
-type type() :: string | binary().
-type parse_fun() :: fun((string() | binary()) -> term()).

%% Callbacks
-callback init(Args :: [term()]) -> {ok, Opts :: options()}.

%% === %%
%% API %%
%% === %%

-spec start_link(Mod :: module(), Args :: [any()]) -> {ok, pid()}.
start_link(Mod, Args) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [Mod, Args])}.

-spec fetch(Pid :: downloader()) -> term() | {error, timeout}.
fetch(Pid) ->
    fetch(Pid, timer:seconds(5)).

-spec fetch(Pid :: downloader(), Timeout :: timeout())
           -> term() | {error, timeout}.
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

-spec init(Mod :: module(), Args :: any())
          -> {ok, reference(), any()} | none().
init(Mod, Args) ->
    {ok, Options} = Mod:init(Args),
    Url = get_url(Options),
    Type = get_type(Options),
    ParseFun = get_parse_fun(Options),
    Timeout = get_timeout(Options),
    HTTPOpts = get_http_options(Options),
    download(Url, Type, ParseFun, Timeout, HTTPOpts).

-spec download(Url :: url(), Type :: type(), ParseFun :: parse_fun(),
               Timeout :: timeout(), HTTPOpts :: http_options())
              -> {ok, reference(), any()} | none().
download(Url, Type, ParseFun, Timeout, HTTPOpts) ->
    {ok, {_, Headers, Result0}} = httpc:request(get, {Url, []},
                                         HTTPOpts, [{body_format, Type}]),
    Result = case is_gzipped(Headers) of
                 true ->
                     zlib:gunzip(Result0);
                 false ->
                     Result0
             end,
    ParsedResult = ParseFun(Result),

    receive
        {fetch, From, Ref} ->
            From ! {ok, Ref, ParsedResult}
    after Timeout ->
            exit(timeout)
    end.

is_gzipped(Headers) ->
    proplists:get_value("content-encoding", Headers) =:= "gzip".

-spec get_url(Optlist :: options()) -> url().
get_url(Optlist) ->
    proplists:get_value(url, Optlist).

-spec get_type(Optlist :: options()) -> type().
get_type(Optlist) ->
    proplists:get_value(type, Optlist, string).

-spec get_parse_fun(Optlist :: options()) -> parse_fun().
get_parse_fun(Optlist) ->
    proplists:get_value(parse_fun, Optlist, fun(Data) -> Data end).

-spec get_timeout(Optlist :: options()) -> timeout().
get_timeout(Optlist) ->
    proplists:get_value(timeout, Optlist, timer:seconds(15)).

-spec get_http_options(Optlist :: options()) -> http_options().
get_http_options(Optlist) ->
    proplists:get_value(http_options, Optlist, []).
