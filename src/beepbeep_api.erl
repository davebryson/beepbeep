%% 
%% Helper methods...more to come
%% 
-module(beepbeep_api).
-author('Dave Bryson <http://weblog.miceda.org>').

-export([set_session/3,get_session/2,get_param/2]).
-include("beepbeep.hrl").

%% Set a Key:Value in the Session
set_session(Params,Key,Value) ->
    beepbeep_session_server:set_session_data(Params#params.sid,Key,Value).

%% Get a value from the session by key
get_session(Params,Key) ->
    Data = beepbeep_session_server:get_session_data(Params#params.sid),
    proplists:get_value(Key,Data).

%% Get a value from the request params for a given key
get_param(Params,Key) ->
    case Key of 
	id ->
	    Params#params.id;
	_Any ->
	    Data = Params#params.data,
	    proplists:get_value(Key,Data)
    end.
