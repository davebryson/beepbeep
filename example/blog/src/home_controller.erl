%%
%% Entry point of the app
%%
-module(home_controller,[Env]).

-export([handle_request/2,before_filter/0]).

%% Index page, pulls data from the DB and sends to index.html
handle_request("index",[]) ->
    Posts = blog_database:latest(),
    %% Put the list of posts and bind to 'data' see the index.html
    {render,"home/index.html",[{data,Posts}]};

%% Simple return the form
handle_request("new",[]) ->
    {render,"home/new.html",[]};

%% Pull post parameters from the form, insert into the db and
%% redirect to the front page
handle_request("create",[]) ->
    Title = beepbeep_args:get_param("post_title",Env),
    Body = beepbeep_args:get_param("post_body",Env),

    %% Example of validation. Require a Title
    if 
	Title =:= undefined orelse length(Title) =:= 0 ->
	    %% Blank
	    %% Set flash message for the template
	    beepbeep_args:flash({notice,"Title Required!"},Env),
	    {redirect,"/home/new"};
	true ->
	    %% Set flash message for the template
	    beepbeep_args:flash({notice,"Post Created!"},Env),
	    blog_database:insert(Title,Body),
	    {redirect,"/"}
    end.

%% Shows how to filter on certain actions in this controller
before_filter() ->
    %% Specify that we only want to filter on 'new' and 'create'
    FilterOnly = ["new","create"],
    %% Check what the current action is. BeepBeep stores the requested
    %% action for access
    case lists:member(beepbeep_args:get_action(Env),FilterOnly) of
	true ->
	    %% Check the session for the key 'user_id'
	    %% if it's there, you are already logged in
	    %% otherwise send you to the login page
	    case beepbeep_args:get_session_data("user_id",Env) of
		undefined ->
		    {redirect,"/login/new"};
		_A -> ok
	    end;
	false ->
	    ok
    end.

    
