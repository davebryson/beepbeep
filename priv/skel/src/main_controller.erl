%% Sample Root Controller

-module(main_controller).

-export([index/1]).
-export([handle_request/2, before_filter/1]).

-behaviour(gen_controller).
-include("beepbeep.hrl").


index(Params) ->
    gen_controller:call(?MODULE,index,Params).


%% Callbacks
handle_request(index,Params) ->
    S = Params#params.sid,
    {render, "main/index.html",[{sess_key,S}],Params}.


%% Callback filter
before_filter(Params) ->
    {ok}.
