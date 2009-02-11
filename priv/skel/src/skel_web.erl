-module(skel_web).
-author('Dave Bryson <http://weblog.miceda.org>').

-export([start/1, stop/0, loop/1]).
-include("beepbeep.hrl").

start(Options) ->
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    %% Setup env...
    InitialEnv = mochiweb_env:setup_environment(Req),
    Env = setup_session(Req,InitialEnv),
    %%error_logger:info_report(Env),
    
    case beepbeep:dispatch(Env) of
	{ok,Status,ContentType,H,Content} ->
	    Cookie = get_cookie(Env), 
	    Headers = [Cookie|H],
	    Req:respond({Status,[{"Content-Type",ContentType}|Headers],Content});
	    %%Req:ok({"text/html",Headers,Content});
	{redirect,Url} ->
	    Req:respond({302, 
                         [{"Location", Url}, 
                          {"Content-Type", "text/html; charset=UTF-8"}], 
                         ""});
	{static, File} ->
	    "/" ++ StaticFile = File,
	    Req:serve_file(StaticFile,skel_deps:local_path(["www"]));
	{error,_} ->
	    Req:respond({500,[],"Server Error"})
    end.
 

get_cookie(Env) ->
    mochiweb_cookies:cookie(?BEEPBEEP_SID,beepbeep_args:get_session_id(Env),[{path, "/"}]).

setup_session(Req,Env) ->
    SessionKey = beepbeep_session_server:new_session(Req:get_cookie_value(?BEEPBEEP_SID)),
    beepbeep_args:set_session_id(SessionKey,Env).
