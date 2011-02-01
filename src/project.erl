%% @author Curtis Carter <curtis@rubyhq.com>. 
%% @doc Information about the project that the tests are being run from<br/>
%% None of these functions should be called on remote nodes.<br/>
%% They are specific to the application who's tests are being ran

-module(project).
-export([project_name/0, remote_path/0, remote_global_setup_script_path/0, local_global_setup_scrip_path/0]).
-vsn("0.0.3").

-define(LOCAL_GLOBAL_SETUP_SCRIPTS, "/etc/distributest/global_setup_scripts").

%% @doc Uses CWD to get the Ruby projects name
project_name() ->
	{ok, ProjectDir} = file:get_cwd(),
  lists:last(string:tokens(ProjectDir, "/")).
	
%% @doc Returns the path that the Ruby project will be copied to. <br/>
%% Currently rsyncing even locally so this is technically the only path local or remote
remote_path() ->
	{ok, LocalHostname} = inet:gethostname(),
	configuration:remote_dir() ++ LocalHostname ++ "/" ++ project_name().

%% @doc Path that the /etc/global_setup_scripts/* gets copied to. <br/>
%% This is based off the local hostname to keep everyones scripts separate
remote_global_setup_script_path() ->
  {ok, LocalHostname} = inet:gethostname(),
  "/tmp/" ++ LocalHostname ++ "/global_setup_scripts".

%% @doc Path that the local global setup scripts are located. ie. node_setup and runner_setup
local_global_setup_scrip_path() ->
	?LOCAL_GLOBAL_SETUP_SCRIPTS.