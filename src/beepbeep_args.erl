%% @author Dave Bryson [http://weblog.miceda.org]
%% @copyright Dave Bryson 2008-2009
%%
%% @doc Helper functions for accessing request and session information
%% in your controllers. 
%%
-module(beepbeep_args).
-export([path/1,
	 path_components/1,
	 server_software/1,
	 server_name/1,
	 server_protocol/1,
	 server_port/1,
	 method/1,
	 content_type/1,
	 content_length/1,
	 remote_addr/1,
	 get_all_headers/1,
	 get_param/2,
	 get_session_id/1,
	 set_session_id/2,
	 set_session_data/3,
	 get_session_data/2,
	 remove_session_data/2,
	 get_all_session_data/1,
	 get_action/1,
	 set_action/2,
	 flash/2,
	 get_flash/1]).

-author('Dave Bryson <http://weblog.miceda.org>').

%%
%% @doc Return the Path
%% 
path(Env) ->
ok,
    proplists:get_value("PATH_INFO",Env).

%%
%% @doc Return the Path as an array parsed on the "/"
%%
path_components(Env) ->
    string:tokens(path(Env),"/").

%%
%% @doc Return the name of the server
%%
server_software(Env) ->
    proplists:get_value("SERVER_SOFTWARE", Env).

%%
%% @doc Return the hostname of the server
%%
server_name(Env) ->
    proplists:get_value("SERVER_NAME", Env).

%%
%% @doc Return the protocol
%%
server_protocol(Env) ->
    proplists:get_value("SERVER_PROTOCOL", Env).

%%
%% @doc Return the Server port
%%
server_port(Env) ->
    proplists:get_value("SERVER_PORT", Env).

%%
%% @doc Return the request method: GET,PUT,POST,DELETE
%%
method(Env) ->
    proplists:get_value("REQUEST_METHOD", Env).

%% 
%% @doc Return the content-type
%%
content_type(Env) ->
    proplists:get_value("CONTENT_TYPE", Env).

%%
%% @doc Return the content_length
%%
content_length(Env) ->
    proplists:get_value("CONTENT_LENGTH", Env).

%%
%% @doc Return the Remote address of the client
%%
remote_addr(Env) ->
    proplists:get_value("REMOTE_ADDR", Env).

%%
%% @doc Return all Headers
%%
get_all_headers(Env) ->
    lists:foldl(fun({"HTTP_" ++ _, _}=Pair, Hdrs) ->
                        [Pair|Hdrs];
                   (_, Hdrs) ->
                        Hdrs
                end, [], Env).

%% 
%% @doc Return a request Value for a given Key. This contains information
%% from a form POST OR GET query string
%%
get_param(Key,Env) ->
    Params = proplists:get_value("beepbeep.data",Env),
    proplists:get_value(Key,Params).

%%
%% @doc Set a 'flash' message for use in your template. All flash message are wrapped in a List
%% 
flash(Term,Env) ->
    Flash = case get_session_data(beepbeep_flash,Env) of
		undefined ->
		    [Term];
		ExistingFlash ->
		    [Term|ExistingFlash]
	    end,
    set_session_data(beepbeep_flash,Flash,Env).


%% Get and clear the flash
get_flash(Env) ->
    Sid = get_session_id(Env),
    case get_session_data(beepbeep_flash,Env) of
	undefined ->
	    %% No flash data
	    none;
	Data ->
	    beepbeep_session_server:remove_session_data(Sid,beepbeep_flash),
	    Data
    end.


%%
%% @doc Get the current session id
%%
get_session_id(Env) ->
    proplists:get_value("beepbeep_sid",Env).

%% 
%% Sets the session id. This is done internally
%% and should not be used manually
%% @hidden
%%  
set_session_id(Value,Env) ->
    case lists:keysearch("beepbeep_sid",1,Env) of
	{value,_} ->
	    set_value("beepbeep_sid",Value,Env);
	false ->
	    [proplists:property({"beepbeep_sid", Value})|Env]
    end.

%% 
%% @doc Set a Key,Value in the session
%%
set_session_data(Key,Value,Env) ->
    Sid = get_session_id(Env),
    beepbeep_session_server:set_session_data(Sid,Key,Value).

%%
%% @doc Return all session data
%%
get_all_session_data(Env) ->
    Sid = get_session_id(Env),
    beepbeep_session_server:get_session_data(Sid).

%%
%% @doc Get the session data for a given key
%%
get_session_data(Key,Env) ->
    proplists:get_value(Key,get_all_session_data(Env)).


%%
%% @doc Remove a Key,Value in the session
%%
remove_session_data(Key,Env) ->
    Sid = get_session_id(Env),
    beepbeep_session_server:remove_session_data(Sid,Key).

%%
%% @doc Return the current requested action
%%
get_action(Env) ->
    proplists:get_value("action_name",Env).

%%
%% @doc Warning! Should not be set manually. This is 
%% done automatically in the dispather.
%% @hidden
%%
set_action(Env,Value) ->
    case lists:keysearch("action_name",1,Env) of
	{value,_} ->
	    set_value("action_name",Value,Env);
	false ->
	    [proplists:property({"action_name", Value})|Env]
    end.

%% 
%% @doc Set an Key,Value in the environment.
%% Used internally
%% @hidden
set_value(Key, Val, Env) ->
     lists:keyreplace(Key, 1, Env, {Key, Val}).

%% get_value(Key, Env) ->
%%     proplists:get_value(Key, Env).

%% get_value(Key, Env, Default) ->
%%     proplists:get_value(Key, Env, Default).

%% get_all_values(Key, Env) ->
%%     proplists:get_all_values(Key, Env).

