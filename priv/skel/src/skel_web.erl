-module(skel_web).
-author('Dave Bryson <http://weblog.miceda.org>').

-export([start/1, stop/0, loop/2]).

-include("beepbeep.hrl").

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
		   ?MODULE:loop(Req, DocRoot)
	   end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    Path = Req:get(path),
    RequestMethod = Req:get(method),
    Params = case RequestMethod of
		 Method when Method =:= 'GET'; Method =:= 'HEAD' ->
		     Req:parse_qs(); 
		 _ ->
		     Req:parse_post()
	     end,
    
    %% Setup the Session
    CookieKey = beepbeep_session_server:new_session(Req:get_cookie_value(?SID)),
    %% Setup Params structure for controllers
    P1 = #params{sid=CookieKey,data=Params,method=RequestMethod},
  

    %% Route the request
    case dispatch(Path,P1) of
	{render,Template,Data,P} ->
	    H = mochiweb_cookies:cookie(?SID,P#params.sid, [{path, "/"}]), 
	    {ok,Content} = render_template(Template,Data),
	    Req:ok({"text/html",[H],Content});
	{redirect,Url} ->
	    Req:respond({302, 
                         [{"Location", Url}, 
                          {"Content-Type", "text/html; charset=UTF-8"}], 
                         ""});
	{static,File} ->
	    Req:serve_file(File,DocRoot)
    end.
    
%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%% Route the Request
dispatch(Path,Params) ->
    PathParts = string:tokens(Path,"/"),
    P1 = extract_route_info(PathParts,Params),
    Controller = P1#params.controller,
    Action = P1#params.action,
    
    Reply = case catch(Controller:Action(P1)) of
		{'EXIT', {undef, _}} ->
		    %% Try static 
		    "/" ++ StaticPath = Path,
		    {static,StaticPath};
		Any ->
		    io:format("Ran with ~p~n",[Any]),
		    Any
	    end,
    Reply.


%% Parse out the route information from the path 
extract_route_info([],P) ->
    %% Default route of root ("/") path
    P1 = P,
    P1#params{controller=main_controller,action=index};
extract_route_info([C],P) ->
    %% Default "/hello" to "hello_controller:index"
    P1 = P,
    P1#params{controller=make_controller_name(C),action=index};
extract_route_info([C,A],P) ->
    P1 = P,
    P1#params{controller=make_controller_name(C),action=list_to_atom(A)};
extract_route_info([C,A,Id],P) ->
    P1 = P,
    P1#params{controller=make_controller_name(C),action=list_to_atom(A), id=Id}.

make_controller_name(ControllerName) ->
    list_to_atom(ControllerName ++ "_controller").


%% Render the Template via erlydtl
render_template(File,Data) ->
    FullPathToFile = skel_deps:local_path(["templates",File]),

    %% Make a module name
    F = lists:reverse(string:tokens(File,"/")),
    [N,_] = string:tokens(hd(F),"."),
    Mn = string:join([N,"template"],"_"),
    ModName = list_to_atom(Mn),

    erlydtl:compile(FullPathToFile,ModName),
    ModName:render(Data).
