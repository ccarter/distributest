-module(configuration).
-export([runner_settings/0, remote_dir/0, test_files_glob/0, settings/0]).

-define(CONF_FILE, "distributest/config.txt").
-record(settings, {hosts, remote_dir, test_files}).

settings_from_file() ->
	case file:consult(?CONF_FILE) of
	{ok, X} ->
		X;
	{error, enoent} ->
		{error, {bad_config_file, ?CONF_FILE}};
	Other ->
		{error, Other}
	end.
	
settings() ->
	SettingsRecord = #settings{hosts=[],test_files=[]},
	settings(settings_from_file(), SettingsRecord).
	
settings([], SettingsRecord) -> 
  SettingsRecord;
settings([H|T], SettingsRecord) ->
	case H of
		{{host, _}, {runner_count, _}} ->
		  NewRecord = SettingsRecord#settings{hosts=[H|SettingsRecord#settings.hosts]},
		  settings(T, NewRecord);
		{remote_dir, Dir} -> 
		  NewRecord = SettingsRecord#settings{remote_dir=Dir},
		  settings(T, NewRecord);
		{test_files, TestFiles} ->
		  NewRecord = SettingsRecord#settings{test_files=[TestFiles|SettingsRecord#settings.test_files]},
  		settings(T, NewRecord)
	end.
	
runner_settings() ->
  SettingsRecord = settings(),
  SettingsRecord#settings.hosts.

remote_dir() ->
	SettingsRecord = settings(),
	SettingsRecord#settings.remote_dir ++ "/".
	
	%%%FINISH HERE WITH RECORD AND SUPPORT MULTI FILE PATHS IN CONFIG
%TODO Allow multiple sets of test files
test_files_glob() ->
	SettingsRecord = settings(),
	test_files_glob(SettingsRecord#settings.test_files, []).
	
test_files_glob([], Globs) -> Globs;
test_files_glob([TestFileGlob|T], Acc) ->
	{ok, Cwd} = file:get_cwd(),
	%Currently forcing to always use relative path in config. TODO to support both or re-design to always be relative
	%through the entire app
  test_files_glob(T, [Cwd ++ "/" ++ TestFileGlob|Acc]).
		
	
	