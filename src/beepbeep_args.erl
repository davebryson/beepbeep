%%
%% Main BeepBeep API to use in Controllers
%%
-module(beepbeep_args).
-compile(export_all).
-author('Dave Bryson <http://weblog.miceda.org>').

%% @spec path(Env) -> Path
%% @doc return the path
path(Env) ->
    proplists:get_value("PATH_INFO",Env).

%% @spec path_components(Env) -> []
%% @doc return the path as an array parsed on the "/"
path_components(Env) ->
    string:tokens(path(Env),"/").

%% @spec server_software(Env) -> ServerSoftware
%% @doc return the name of the server
server_software(Env) ->
    proplists:get_value("SERVER_SOFTWARE", Env).

%% @spec server_name(Env) -> ServerName
%% @doc return the hostname of the server
server_name(Env) ->
    proplists:get_value("SERVER_NAME", Env).

%% @spec server_protocol(Env) -> ServerProtocol
%% @doc return the protocol i.e. http
server_protocol(Env) ->
    proplists:get_value("SERVER_PROTOCOL", Env).

server_port(Env) ->
    proplists:get_value("SERVER_PORT", Env).

method(Env) ->
    proplists:get_value("REQUEST_METHOD", Env).

content_type(Env) ->
    proplists:get_value("CONTENT_TYPE", Env).

content_length(Env) ->
    proplists:get_value("CONTENT_LENGTH", Env).

remote_addr(Env) ->
    proplists:get_value("REMOTE_ADDR", Env).

get_all_headers(Env) ->
    lists:foldl(fun({"HTTP_" ++ _, _}=Pair, Hdrs) ->
                        [Pair|Hdrs];
                   (_, Hdrs) ->
                        Hdrs
                end, [], Env).

get_param(Key,Env) ->
    Params = proplists:get_value("beepbeep.data",Env),
    proplists:get_value(Key,Params,"?").

get_session_id(Env) ->
    proplists:get_value("beepbeep_sid",Env).

set_session_id(Value,Env) ->
    case lists:keysearch("beepbeep_sid",1,Env) of
	{value,_} ->
	    set_value("beepbeep_sid",Value,Env);
	false ->
	    [proplists:property({"beepbeep_sid", Value})|Env]
    end.

%% Helpers for accessing Session Data
set_session_data(Env,Key,Value) ->
    Sid = get_session_id(Env),
    beepbeep_session_server:set_session_data(Sid,Key,Value).

get_session_data(Env) ->
    Sid = get_session_id(Env),
    beepbeep_session_server:get_session_data(Sid).

get_action(Env) ->
    proplists:get_value("action_name",Env).

set_action(Env,Value) ->
    case lists:keysearch("action_name",1,Env) of
	{value,_} ->
	    set_value("action_name",Value,Env);
	false ->
	    [proplists:property({"action_name", Value})|Env]
    end.

get_value(Key, Env) ->
    proplists:get_value(Key, Env).

get_value(Key, Env, Default) ->
    proplists:get_value(Key, Env, Default).

get_all_values(Key, Env) ->
    proplists:get_all_values(Key, Env).

set_value(Key, Val, Env) ->
    lists:keyreplace(Key, 1, Env, {Key, Val}).
