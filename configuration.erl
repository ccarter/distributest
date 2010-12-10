-module(configuration).
-compile(export_all).
-define(CONF_FILE, "configuration/config.txt").

runner_settings() ->
	case file:consult(?CONF_FILE) of
	{ok, X} ->
		X;
	{error, enoent} ->
		{error, {bad_config_file, ?CONF_FILE}};
	Other ->
		{error, Other}
	end.
	
