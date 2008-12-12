
%% Session Id key
-define(SID,"_beepbeep_session_id").

%% Holds parameter information passes around
-record(params,{
	  controller,
	  action,
	  id,
	  method,
	  data, %% Query and Post Data
 	  sid   %% Session Id
	 }).
