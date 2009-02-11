%% Session Id key
-define(BEEPBEEP_SID, "_beepbeep_session_id_").

%% Environment data
-define(BEEPBEEP_ENV_DATA, [{server_sw, "SERVER_SOFTWARE"},
			    {server_name, "SERVER_NAME"},
			    {server_protocol, "SERVER_PROTOCOL"},
			    {server_port, "SERVER_PORT"},
			    {method, "REQUEST_METHOD"},
			    {content_type, "CONTENT_TYPE"},
			    {content_length,"CONTENT_LENGTH"},
			    {path_info, "PATH_INFO"},
			    {remote_addr, "REMOTE_ADDR"},
			    {beepbeep_params, "beepbeep.data"}]).
