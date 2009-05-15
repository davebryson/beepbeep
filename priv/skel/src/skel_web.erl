%% @author Dave Bryson [http://weblog.miceda.org]
%% @copyright Dave Bryson 2008-2009
%% 
%% Creates a MochiWeb Server with the BeepBeep hook
%%
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
    

    %% Possible return values
    %% {render,View,Data}
    %% {render,View,Data,Options}
    %% {text,Data}
    %% {json,Data}
    %% {redirect,Url}
    %% {static,File}
    %% {error,_} 
    case beepbeep:dispatch(Env) of
	{render,View,Data} ->
	    {ok,Content} = render_template(View,Data,Env),
	    Req:respond({200,
			 [{"Content-Type","text/html"}|[get_cookie(Env)]],
			 Content});
	{render,View,Data,Options} ->
	    {Status,ContentType,Headers} = extract_options(Options),
	    {ok,Content} = render_template(View,Data,Env),
	    Req:respond({Status,
			 [{"Content-Type",ContentType}|[get_cookie(Env)|Headers]],
			 Content});
	{text,Content} ->
	    Req:respond({200,
			 [{"Content-Type","text/plain"}|[get_cookie(Env)]],
			 Content});
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

render_template(ViewFile,Data,Env) -> 
    %% Copy flash into Data and clear from Session
    Data1 = set_and_clear_flash(Data,Env),
    FullPathToFile = filename:join([skel_deps:local_path(["views"]),ViewFile]),
    beepbeep:render_template(FullPathToFile,ViewFile,Data1).

extract_options(Options) ->
    {proplists:get_value(status,Options,200),
     proplists:get_value(content_type,Options,"text/html"),
     proplists:get_value(headers,Options,[])}.

get_cookie(Env) ->
    mochiweb_cookies:cookie(?BEEPBEEP_SID,beepbeep_args:get_session_id(Env),[{path, "/"}]).

set_and_clear_flash(Data,Env) ->
    case beepbeep_args:get_flash(Env) of
	none -> Data;
	Flash ->
	    [{flash,Flash}|Data]
    end.

setup_session(Req,Env) ->
    SessionKey = beepbeep_session_server:new_session(Req:get_cookie_value(?BEEPBEEP_SID)),
    beepbeep_args:set_session_id(SessionKey,Env).
