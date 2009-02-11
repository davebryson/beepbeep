%%%-------------------------------------------------------------------
%%% Author  : Dave Bryson <http://weblog.mitre.org>
%%%
%%% Description : Maps the path of the controller to the actual controller 
%%% name as an atom. Prevents calling list_to_atom in the dispatcher which
%%% could lead to a potential DOS attack.  Thanks to Ville for catching that 
%%%
%%%-------------------------------------------------------------------
-module(beepbeep_router).
-author('Dave Bryson <http://weblog.miceda.org>').

-behaviour(gen_server).

%% API
-export([start/1,get_controller/1,view_map/0,get_view/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,{view_path,controllers}).

%% 
%% Start the app with the Basedir of the application
%% Basedir is determined in the webapp supervisor
%%
start(BaseDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, BaseDir, []).

%%
%% Given the name of the controller called in the URL
%% return the module name
%%
get_controller(Controller) ->
    gen_server:call(?MODULE,{get_controller,Controller}).

%%
%% Return the fullpath and name of the requested template
%%
get_view(Name) ->
    gen_server:call(?MODULE,{get_view,Name}).

%%
%% Simple helper to view the name-controller mapping
%%
view_map() ->
    gen_server:call(?MODULE,view).

init(BaseDir) ->
    %% Get the base directory for views
    ViewPath = filename:join([BaseDir,"views"]),
    %% Load the controllers
    Controllers = load_controllers(BaseDir),
    State = #state{view_path=ViewPath,controllers=Controllers},
    {ok, State}.

%% ---------Callbacks------------------------------
handle_call({get_controller,Controller},_From, State) ->
    ControllerList = State#state.controllers,
    Reply = case lists:keysearch(Controller,1,ControllerList) of
		{value,{_,C}} ->
		    {ok,C};
		false ->
		    no_controller
	    end,
    {reply, Reply, State};

handle_call({get_view,Name},_From,State) ->
    Base = State#state.view_path,
    TemplatePath = filename:join([Base,Name]),
    {reply,TemplatePath,State};

handle_call(view,_From,State) ->
    ControllerList = State#state.controllers,
    io:format("~p~n",[ControllerList]),
    {reply,ok,State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
list_controllers(BaseDir) ->
    %%{file, Here} = code:is_loaded(?MODULE),
    %%BaseDir = filename:dirname(filename:dirname(Here)),
    Path = filename:join([BaseDir,"src","*_controller.erl"]),
    filelib:wildcard(Path).
    

load_controllers(BaseDir) ->
    lists:foldl(fun(File,Acc) ->
			OrgName = filename:basename(File,".erl"),
			{ok,KeyName,_} = regexp:gsub(OrgName,"_controller",""), 
			AtomName = list_to_atom(OrgName),
			[{KeyName,AtomName}|Acc]
		end,
		[],
		list_controllers(BaseDir)).
    
				      
