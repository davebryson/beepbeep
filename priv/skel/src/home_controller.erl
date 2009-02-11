%%
%% Sample default controller
%%
-module(home_controller,[Env]).

-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"home/index.html",[{data,"Hello There From BeepBeep!"}]};

handle_request("show",[Year]) ->
    Sid = beepbeep_args:get_session_id(Env),
    Name = beepbeep_args:get_param("name",Env),
    {render,"home/show.html",[{year,Year},{sid,Sid},{name,Name}]}.


before_filter() ->
    FilterOnly = ["show"],
    case lists:member(beepbeep_args:get_action(Env),FilterOnly) of
	true ->
	    error_logger:info_report("Doing the filter for SHOW~n"),
	    ok;
	false ->
	    ok
    end.

    
