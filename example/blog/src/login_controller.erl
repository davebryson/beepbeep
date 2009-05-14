%%  Controls login
-module(login_controller,[Env]).

-export([handle_request/2,before_filter/0]).

%% Return the login form
handle_request("new",[]) ->
    {render,"login/new.html",[]};

%% Get the post params from the form and verify
handle_request("create",[]) ->
    Un = beepbeep_args:get_param("un",Env),
    Pw = beepbeep_args:get_param("pw",Env),

    %% Check if the user entered the proper Username and Password. 
    %% In this case it's hard code - foo:foobar
    case Un =:= "foo" andalso Pw =:= "foobar" of
	true ->
	    %% If the user entered the correct Un/Pw
	    %% Set the user_id in the session and redirect
	    beepbeep_args:set_session_data("user_id","dave",Env),
	    
	    {redirect,"/home/new"};
	false ->
	    %% Bad username and password - redirect back to the login page
	    {redirect,"/login/new"}
    end.

%% No filter used
before_filter() ->
    ok.
