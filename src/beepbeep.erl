%%
%% Dispatcher called from the mochiweb server
%% Maps Urls to controllers and their views
%%
-module(beepbeep).
-author('Dave Bryson <http://weblog.miceda.org>').

-export([dispatch/1,get_view_file/1]).

dispatch(Env) ->
    PathComponents = beepbeep_args:path_components(Env),
    %% Map the request to our app
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
	    %% Try static
	    F = beepbeep_args:path(Env),
	    {static,  F}
    end.



get_view_file(ViewFile) ->
    %%filename:join([get_base_dir()|["views",ViewFile]]),
    beepbeep_router:get_view(ViewFile).


%%get_static_path() ->
%%    filename:join([get_base_dir()|["www"]]).


%%% Internal below
%%get_base_dir() ->
%%    {file, Here} = code:is_loaded(?MODULE),
%%    filename:dirname(filename:dirname(Here)).

process_request(Env,ControllerName,ActionName,Args) ->
    Env1 = beepbeep_args:set_action(Env,ActionName),
    error_logger:info_report(Env1),
    Controller = ControllerName:new(Env1),
    case try_filter(Controller) of
	ok ->
	    case catch(Controller:handle_request(ActionName,Args)) of
		{'EXIT',_} ->
		    {error,no_action};
		Response ->
		    handle_response(Response)
	    end;
	Any ->
	    Any
    end.

try_filter(ControllerName) ->
    case catch(ControllerName:before_filter()) of
	{'EXIT', {undef,_}} ->
	    ok;
	Any ->
	    Any
    end.

%% Handle all responses from controller
handle_response({render,View,Data}) ->
    {ok,Content} = render_template(View,Data),
    {ok,200,"text/html",[],Content};

handle_response({render,View,Data,Options}) ->
    {ok,Content} = render_template(View,Data),
    {ok,
     proplists:get_value(status,Options,200),
     proplists:get_value(content_type,Options,"text/html"),
     proplists:get_value(headers,Options,[]),
     Content};

handle_response({text,Data}) ->
    {ok,200,"text/plain",[],Data};

%% This seems stupid...better way??
handle_response({redirect,Url}) ->
    {redirect,Url};

handle_response({static,File}) ->
    {static,File}.
    
render_template(ViewFile,Data) ->
    FullPathToFile = get_view_file(ViewFile),
    error_logger:info_msg("Trying file: ~s~n",[FullPathToFile]),
    Pieces = string:tokens(ViewFile,"/"),
    Name = string:join(Pieces,"_"),
    Name1 = filename:basename(Name,".html"),
    ModName = list_to_atom(Name1 ++ "_view"),

    erlydtl:compile(FullPathToFile,ModName),
    ModName:render(Data).    
	    
	    
