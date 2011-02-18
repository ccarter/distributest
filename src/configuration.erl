%% @author Curtis Carter <curtis@rubyhq.com>. 
%% @doc Handles the configuration files.

-module(configuration).
-export([runner_settings/0, remote_dir/0, test_files_glob/0, settings/0, settings_from_file/1, display_file_time_greater_than/0, display_profile_time_greater_than/0, user_for_current_host/0, node_settings_for_host/1]).
-vsn("0.0.4").

-define(GLOBAL_CONF_FILE, "/etc/distributest/config.txt").
-define(PROJECT_CONF_FILE, ".distributest/config.txt").
%%Following are defaults that can be overridden in config.txt
-define(PROFILE_TIME_GREATER_THAN, 1).
-define(FILE_TIME_GREATER_THAN, 20).

-include("includes/configuration.hrl").

%% Use the local user for sshing into remote nodes be default.
%% This is overriden by config.txt if the {ssh_user, String} is present
default_ssh_user() ->
	os:getenv("USER").

%% @doc Loads the settings from the file.<br/> 
%% File must be valid erlang terms.
settings_from_file(File) ->
	case file:consult(File) of
	{ok, X} ->
		X;
	{error, enoent} ->
		io:format("~nMISSING OR BAD CONFIG FILE: ~p~n", [File]),
		exit(missing_config_file);
	Other ->
		{error, Other}
	end.

%% Creates a settings record that has the defaults to be used
default_settings() ->
	#settings{hosts=[],test_files=[],display_file_time_greater_than=?FILE_TIME_GREATER_THAN,display_profile_time_greater_than=?PROFILE_TIME_GREATER_THAN}.

%% @doc Uses a settings record and loads entire config.txt into it
settings() ->
	SettingsRecord = default_settings(),
	NodeSettings = settings_from_file(?GLOBAL_CONF_FILE),
	settings(NodeSettings, SettingsRecord).
	
settings([], SettingsRecord) -> 
  SettingsRecord;
settings([H|T], SettingsRecord) ->
	case H of
		{{host, Host}, {runner_count, RunnerCount}} ->
		  NodeSettings = #node_settings{host=Host,runner_count=RunnerCount,ssh_user=default_ssh_user()},
 		  NewRecord = SettingsRecord#settings{hosts=[NodeSettings|SettingsRecord#settings.hosts]},
		  settings(T, NewRecord);
		{{host, Host}, {runner_count, RunnerCount}, {ssh_user, SshUser}} ->
		  NodeSettings = #node_settings{host=Host,runner_count=RunnerCount,ssh_user=SshUser},
 		  NewRecord = SettingsRecord#settings{hosts=[NodeSettings|SettingsRecord#settings.hosts]},
		  settings(T, NewRecord);
		{remote_dir, Dir} -> 
		  NewRecord = SettingsRecord#settings{remote_dir=Dir},
		  settings(T, NewRecord);
		{test_files, TestFiles} ->
		  NewRecord = SettingsRecord#settings{test_files=[TestFiles|SettingsRecord#settings.test_files]},
  		settings(T, NewRecord);
    {display_file_time_greater_than, Seconds} ->
	    NewRecord = SettingsRecord#settings{display_file_time_greater_than=Seconds},
	    settings(T, NewRecord);
	  {display_profile_time_greater_than, Seconds} ->
		  NewRecord = SettingsRecord#settings{display_profile_time_greater_than=Seconds},
		  settings(T, NewRecord)
	end.
	
%% @doc Returns list of node_settings records for each node in systems config.txt
runner_settings() ->
  SettingsRecord = settings(),
  SettingsRecord#settings.hosts.

user_for_current_host() ->
  case node_settings_for_current_host() of
	  {error, Error} -> {error, Error};
	  {ok, NodeSettings} ->	
	    {ok, NodeSettings#node_settings.ssh_user}
	end.

node_settings_for_current_host() ->
  case os:getenv("DISTRIBUTEST_LOCAL_HOSTNAME") of
  	false -> 
      io:format("~nCouldn't find Environment Variable ~p, so can't lookup node_settings~n", ["DISTRIBUTEST_LOCAL_HOSTNAME"]),
      {error, could_not_find_env_variable};
    Host -> case node_settings_for_host(Host) of
	            {error, Error} -> {error, Error};
	            {ok, Res} -> {ok, Res}
	          end
  end.
	
node_settings_for_host(Host) ->
	case lists:keysearch(Host, #node_settings.host, runner_settings()) of
		false -> 
		  io:format("~nNode Settings for Host:~p not found.~n", [Host]),
		  {error, node_settings_not_found_for_host};
		{value, Record} -> {ok, Record}
	end.

%% @doc Returns the remote directory that is going to be used to copy the Ruby project into
remote_dir() ->
	SettingsRecord = settings(),
	SettingsRecord#settings.remote_dir ++ "/".
	
%% @doc Returns int with configured time in seconds X of what files to show at the
%% end of the test run whos time was greater than X <br/>
%% This is defaulted in this module so the tuple is not required in the config.txt
display_file_time_greater_than() ->
	SettingsRecord = settings(),
	SettingsRecord#settings.display_file_time_greater_than.
	
%% @doc Returns int with configured time in seconds X of what tests to show at the
%% end of the test run whos time was greater than X <br/>
%% This is defaulted in this module so the tuple is not required in the config.txt
display_profile_time_greater_than() ->
	SettingsRecord = settings(),
	SettingsRecord#settings.display_profile_time_greater_than.
	
%% @doc Returns a string that's expected to be a glob of what tests to run <br/>
%% If the project has a .distributest/config.txt it will override this <br/>
%% This allows per project selection of tests to run.
test_files_glob() ->
	FileInfo = file:read_file_info(?PROJECT_CONF_FILE),
  ProjectOverrideSettings = case FileInfo of
		{error, enoent} -> {error, enoent}; %File doesn't exist in project
		{ok, _Fileinfo} -> settings(settings_from_file(?PROJECT_CONF_FILE), default_settings())
	end,
	
	%If Project has config.txt it tries to find any test globs. If it doesn't find any it 
	%goes to the systems config.txt
	SettingsRecord = case ProjectOverrideSettings#settings.test_files of
    {error, enoent} -> settings();
    [] -> settings();
    _OverrideSettings -> ProjectOverrideSettings
  end,			

  case SettingsRecord#settings.test_files of
	  [] ->
		  io:format("Could not find a test files glob in either /etc/distributest/config.txt"
		            "nor in the projects .distributest/config.txt if it exists");
		_Any ->
      test_files_glob(SettingsRecord#settings.test_files, [])
  end.
	
test_files_glob([], Globs) -> Globs;
test_files_glob([TestFileGlob|T], Acc) ->
	{ok, Cwd} = file:get_cwd(),
	%Currently forcing to always use relative path in config. TODO to support both or re-design to always be relative
	%through the entire app
  test_files_glob(T, [Cwd ++ "/" ++ TestFileGlob|Acc]).
		
	
	