%% @author Dave Bryson [http://weblog.miceda.org]
%% @copyright Dave Bryson 2008-2009
%%
%% @doc On start up, this module builds a mapping of the legal Controller
%% requests based on the actually files with names ending in '_controller'
%% in the src directory. For example, if you created two controllers:
%%
%% 'feed_controller' and 'login_controller'
%% 
%% the mapping:
%%
%% feed -> feed_controller
%%
%% login -> login_controller
%%
%% is created.  This prevents creating an atom() for every incoming
%% request which could lead to a potential DoS attack by filling the global
%% atom() table.
%%
-module(beepbeep_router).
-author('Dave Bryson <http://weblog.miceda.org>').

-behaviour(gen_server).

%% API
-export([start/1,get_controller/1,controller_map/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% 
%% @doc Start the app with the Basedir of the application.
%% Basedir is determined in the  supervisor
%%
start(BaseDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,BaseDir,[]).

%%
%% @doc Given the name of the controller in the URL
%% return the module name in the mapping
%%
get_controller(Controller) ->
    gen_server:call(?MODULE,{get_controller,Controller}).

%%
%% @doc Simple helper to view the name-controller mapping
%%
controller_map() ->
    gen_server:call(?MODULE,view).

%% @hidden
init(BaseDir) ->
    {ok, load_controllers(BaseDir)}.

%% ---------Callbacks------------------------------

%% @hidden
handle_call({get_controller,Controller},_From, State) ->
    Reply = case lists:keysearch(Controller,1,State) of
		{value,{_,C}} ->
		    {ok,C};
		false ->
		    no_controller
	    end,
    {reply, Reply, State};

%% @hidden
handle_call(view,_From,State) ->
    error_logger:info_msg("Controller Map:~n~p~n",[State]),
    {reply,ok,State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.
%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.
%% @hidden
terminate(_Reason, _State) ->
    ok.
%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
list_controllers(BaseDir) ->
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
    
				      
