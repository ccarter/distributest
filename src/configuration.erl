-module(configuration).
-export([runner_settings/0, remote_dir/0, test_files_glob/0, settings/0, settings_from_file/1, display_file_time_greater_than/0, display_profile_time_greater_than/0, user_for_current_host/0, node_settings_for_host/1]).
-vsn("0.0.4").

-define(NODE_CONF_FILE, "/etc/distributest/node_config.txt").
-define(TEST_CONF_FILE, "distributest/test_config.txt").
%%Following are defaults that can be overridden in node_config.txt
-define(PROFILE_TIME_GREATER_THAN, 1).
-define(FILE_TIME_GREATER_THAN, 20).

-include("includes/configuration.hrl").

default_ssh_user() ->
	os:getenv("USER").

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

default_settings() ->
	#settings{hosts=[],test_files=[],display_file_time_greater_than=?FILE_TIME_GREATER_THAN,display_profile_time_greater_than=?PROFILE_TIME_GREATER_THAN}.

settings() ->
	SettingsRecord = default_settings(),
	NodeSettings = settings_from_file(?NODE_CONF_FILE),
	%TestSettings = settings_from_file(?TEST_CONF_FILE),
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

remote_dir() ->
	SettingsRecord = settings(),
	SettingsRecord#settings.remote_dir ++ "/".
	
display_file_time_greater_than() ->
	SettingsRecord = settings(),
	SettingsRecord#settings.display_file_time_greater_than.
	
display_profile_time_greater_than() ->
	SettingsRecord = settings(),
	SettingsRecord#settings.display_profile_time_greater_than.
	
%%If the project has TEST_CONF_FILE it overrides the one in NODE_CONF_FILE
test_files_glob() ->
	FileInfo = file:read_file_info(?TEST_CONF_FILE),
	SettingsRecord = case FileInfo of
		{error, enoent} -> settings(); %File doesn't exist in project so not going to override test file glob
		{ok, _Fileinfo} -> settings(settings_from_file(?TEST_CONF_FILE), default_settings())
	end,
	test_files_glob(SettingsRecord#settings.test_files, []).
	
test_files_glob([], Globs) -> Globs;
test_files_glob([TestFileGlob|T], Acc) ->
	{ok, Cwd} = file:get_cwd(),
	%Currently forcing to always use relative path in config. TODO to support both or re-design to always be relative
	%through the entire app
  test_files_glob(T, [Cwd ++ "/" ++ TestFileGlob|Acc]).
		
	
	