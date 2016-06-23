freeloader
=====

A behaviour for HTTP downloads (freeloader_downloader)

Add to your deps
----------------

Add freeloader to your deps in rebar.config

    ...
    {deps, [...,
            freeloader,
            ...]}
    ...

Build
-----

    $ rebar3 compile

Usage
-----

Since it is a behaviour, you can specify that your new module
uses it (`-behaviour(freeloader_downloader).`) and define `init/2`:

```erlang
init(Args) when is_list(Args) ->
    {ok, Opts}.
```

`Opts` in the above tuple is an optionlist which can contain the following:

- `{url, Url}` where `Url` is a `string()` like `"http://example.com/"`
- `{type, Type}` where `Type` is either of atoms `string` or `binary`
- `{parse_fun, ParseFun}` where `ParseFun` is a
  `fun((string() | binary()) -> term())`
- `{timeout, Timeout}` where `Timeout` is a
  `timeout()` (`non_neg_integer()` or the atom `infinite`)
- `{http_options, HTTPOpts}` where `HTTPOpts` is a list of `httpc:http_options()`

Example module
--------------

```erlang
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
```
