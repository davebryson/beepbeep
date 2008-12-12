%%%-------------------------------------------------------------------
%%% Description : Maintains session information for the client. All data is stored
%%% on the server. Only a unique session id is exchanged with the client.
%%% Inspired by the Yaws Session Server.
%%%
%%%-------------------------------------------------------------------
-module(beepbeep_session_server).
-author('Dave Bryson <http://weblog.miceda.org>').


-behaviour(gen_server).

-export([start/0,new_session/1,get_session_data/1,set_session_data/3,delete_session/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-record(beep_session, {sid,data,ttl}).


%%% API
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE,[set,named_table,{keypos, 2}]),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    {ok, undefined}.

new_session(Data) ->
    gen_server:call(?MODULE,{new_session,Data}).

get_session_data(Sid) ->
    gen_server:call(?MODULE,{get_session_data,Sid}).

set_session_data(Sid,Key,Value) ->
    gen_server:call(?MODULE,{set_session_data,Sid,Key,Value}).

delete_session(Sid) ->
    gen_server:call(?MODULE,{delete_session,Sid}).


%%% Callbacks
handle_call({new_session,Cookie}, _From, _State) ->
    NewId = case Cookie of
		undefined ->
		    make_session();
		Any ->
		    case ets:member(?MODULE, Any) of
			true -> Any;
			false -> make_session()
		    end
	    end,
    {reply,NewId,undefined};

handle_call({get_session_data,Sid},_From,_State) ->
    Data = case ets:lookup(?MODULE, Sid) of
	       [S] ->
		   S#beep_session.data;
	       [] ->
		   []
	   end,
    {reply,Data,undefined};

handle_call({set_session_data,Sid,Key,Value},_From,_State) ->
    Data = case ets:lookup(?MODULE,Sid) of
	       [S] ->
		   S#beep_session.data;
	       [] -> []
	   end,
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    Rest = proplists:delete(Key,Data),
		    [{Key,Value}|Rest];
		false ->
		    [{Key,Value}|Data]
	    end,
    
    ets:insert(?MODULE,#beep_session{sid=Sid,data=Data1,ttl=0}),

    {reply,ok,undefined};


handle_call({delete_session,Sid},_From,_State) ->
    ets:delete(?MODULE,Sid),
    {reply,ok,undefined}.


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

make_session() ->
    Data = crypto:rand_bytes(2048),
    Sha_list = binary_to_list(crypto:sha(Data)),
    Id = lists:flatten(list_to_hex(Sha_list)),
    Session = #beep_session{sid=Id,data=[],ttl=0},
    ets:insert(?MODULE,Session),
    Id.

%% Convert Integer from the SHA to Hex
list_to_hex(L)->
       lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 -> 
       [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
