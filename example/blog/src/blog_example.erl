-module(blog_example).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the blog_example server.
start() ->
    blog_example_deps:ensure(),
    ensure_started(crypto),
    application:start(blog_example).

%% @spec stop() -> ok
%% @doc Stop the blog_example server.
stop() ->
    Res = application:stop(blog_example),
    application:stop(crypto),
    Res.
