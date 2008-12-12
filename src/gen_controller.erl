%% This behavior should be used by all controllers.
%% It provides the hook for before_filters and others
%% in the future. 

-module(gen_controller).
-author('Dave Bryson <http://weblog.miceda.org>').

-export([call/3]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle_request,2},{before_filter,1}];
behaviour_info(_Other) ->
    undefined.

%% 
%% Called from the controller. Always does the filter
%% then the actual request.
%% 
call(M,A,Params) ->
    case M:before_filter(Params) of
	{ok} ->
	    M:handle_request(A,Params);
	Any -> Any
    end.
    
