-module(project).
-export([project_name/0, remote_path/0]).

%%Can not be called from remote runner because of cwd
project_name() ->
	{ok, ProjectDir} = file:get_cwd(),
  lists:last(string:tokens(ProjectDir, "/")).
	
%%Can not be called from remote runner because of cwd
remote_path() ->
	{ok, LocalHostname} = inet:gethostname(),
	configuration:remote_dir() ++ LocalHostname ++ "/" ++ project_name().