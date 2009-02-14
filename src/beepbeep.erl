%% @author Dave Bryson [http://weblog.miceda.org]
%% @copyright Dave Bryson 2008-2009
%%
%% @doc BeepBeep Dispatcher
%%
%% This is called from the MochiWeb server to dispatch
%% requests. Requests are mapped to modules (controllers) and 
%% functions (actions) based on path components in the Url. For
%% example, the request:
%%
%% '/feed/show'
%%
%% would get mapped to the module (controller) 'feed' and the function 
%% (action) 'show'. 
%% By default the root request:
%% 
%% '/'
%% 
%% is automatically mapped to the module 'home' and the function 'index'     
%% Maps Urls to controllers and their views
%%
-module(beepbeep).
-author('Dave Bryson <http://weblog.miceda.org>').

-export([dispatch/1,render_template/3]).

%%
%% @doc Dispatches the incoming request to the proper module
%% and function and returns the tuple() from the controller. 
%%
%% @spec dispatch(Env::record()) -> tuple()
%%
dispatch(Env) ->
    PathComponents = beepbeep_args:path_components(Env),
    {ControllerName,ActionName,Args}  = case PathComponents of
					    [] ->
						{"home","index",[]};
					    [C] ->
						{C,"index",[]};
					    [C,A | Params]  ->
						{C,A,Params}
					end,
    case beepbeep_router:get_controller(ControllerName) of
	{ok,Controller} ->
	    process_request(Env,Controller,ActionName,Args);
	no_controller ->
	    %% Try static content using the PATH_INFO
	    F = beepbeep_args:path(Env),
	    {static,  F}
    end.


process_request(Env,ControllerName,ActionName,Args) ->
    Env1 = beepbeep_args:set_action(Env,ActionName),
    Controller = ControllerName:new(Env1),
    %%
    %% First try the before_filter. If the call returns 'ok', then try the 
    %% action call. If the action call returns any kind of exit, return
    %% {error,no_action}. Otherwise return the response from the Controller
    %%
    case try_filter(Controller) of
	ok ->
	    case catch(Controller:handle_request(ActionName,Args)) of
		{'EXIT',_} ->
		    {error,no_action};
		Response -> Response
	    end;
	Any ->
	    Any
    end.

try_filter(ControllerName) ->
    case catch(ControllerName:before_filter()) of
	{'EXIT', {undef,_}} -> ok;
	Any -> Any
    end.

%%
%% @doc Render the template with the given data.
%% This is called from YOUR_APP_web.erl automatically.
%%    
render_template(FullPathToFile,ViewFile,Data) ->
    Pieces = string:tokens(ViewFile,"/"),
    Name = string:join(Pieces,"_"),
    Name1 = filename:basename(Name,".html"),
    ModName = list_to_atom(Name1 ++ "_view"),

    erlydtl:compile(FullPathToFile,ModName),
    ModName:render(Data).    
	    
	    
