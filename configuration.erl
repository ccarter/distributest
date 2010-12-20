-module(configuration).
-export([runner_settings/0, remote_dir/0, test_files_glob/0]).
%figure out where proper place for this is in final product. 
-define(CONF_FILE, "/Users/testadmin/distributed_testing/erlang/configuration/config.txt").

%%%TODO. Figure out how to refactor this entire module
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
	settings(settings_from_file(), [], []).
	
settings([], Runners, OtherSettings) -> [{runners, Runners},{other_settings, OtherSettings}];
settings([H|T], Runners, OtherSettings) ->
	case H of
		{{host, _}, {runner_count, _}} ->  settings(T,[H|Runners], OtherSettings);
		{remote_dir, _} -> settings(T, Runners, [H|OtherSettings]);
		{test_files, _} -> settings(T, Runners, [H|OtherSettings])
	end.
	
other_settings() ->
	[{_,_}, {other_settings, OtherSettings}] = settings(),
	OtherSettings.
	
runner_settings() ->
  [{runners, Runners},{_,_}] = settings(),
  Runners.

remote_dir() ->
	remote_dir(other_settings()).

remote_dir([]) -> io:format("{remote_dir, _} not found in config.txt", []);
remote_dir([H|OtherSettings]) ->
	case H of
		{remote_dir, RemoteDir} -> RemoteDir ++ "/";
		{_,_} -> remote_dir(OtherSettings)
	 end.
	
%TODO Allow multiple sets of test files
test_files_glob() ->
	test_files_glob(other_settings()).
	
test_files_glob([]) -> io:format("{test_files, _} not found in config.txt", []);
test_files_glob([H|OtherSettings]) ->
	{ok, Cwd} = file:get_cwd(),
	case H of
		%Currently forcing to always use relative path in config. TODO to support both or re-design to always be relative
		%through the entire app
		{test_files, TestFileGlob } ->  Cwd ++ "/" ++ TestFileGlob;
		{_,_} -> test_files_glob(OtherSettings)
	end.
	
	