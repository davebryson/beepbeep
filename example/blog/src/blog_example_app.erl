-module(blog_example_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for blog_example.
start(_Type, _StartArgs) ->
    blog_example_deps:ensure(),
    blog_example_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for blog_example.
stop(_State) ->
    ok.
