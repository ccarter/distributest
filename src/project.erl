-module(project).
-export([project_name/0, remote_path/0, remote_global_setup_script_path/0, local_global_setup_scrip_path/0]).
-vsn("0.0.3").

-define(LOCAL_GLOBAL_SETUP_SCRIPTS, "/etc/distributest/global_setup_scripts").

%%Can not be called from remote runner because of cwd
project_name() ->
	{ok, ProjectDir} = file:get_cwd(),
  lists:last(string:tokens(ProjectDir, "/")).
	
%%Can not be called from remote runner because of cwd
%%NOTE: Currently rsyncing even locally so this is technically the only path local or remote
remote_path() ->
	{ok, LocalHostname} = inet:gethostname(),
	configuration:remote_dir() ++ LocalHostname ++ "/" ++ project_name().

%should only be run from master
remote_global_setup_script_path() ->
  {ok, LocalHostname} = inet:gethostname(),
  "/tmp/" ++ LocalHostname ++ "/global_setup_scripts".

local_global_setup_scrip_path() ->
	?LOCAL_GLOBAL_SETUP_SCRIPTS.