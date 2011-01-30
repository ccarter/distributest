-module(files).
-include_lib("kernel/include/file.hrl").
-export([test_files/0, file/2, potentially_slow_files/1, log_file_time/1,sorted_test_files/0, get_logged_file_time/0,sorted_files_per_historic_time/0]).
-vsn("0.0.3").

sorted_test_files() ->
	TestFiles = test_files(),
	HistoricFiles = sorted_files_per_historic_time(),
  MatchingFilesSorted = ([X || X <- HistoricFiles, lists:member(X, TestFiles) == true]),
  RemainingFilesToRun = TestFiles -- MatchingFilesSorted,
  lists:append(MatchingFilesSorted, RemainingFilesToRun).

%%All the files to be ran. Can be multiple sets.Sorts per set(on size) not as 1 list
test_files([], Files) -> lists:append(Files);
test_files([FileGlobsHead|T], FilesAcc) ->
	Files = filelib:wildcard(FileGlobsHead),
  SortedForComplexity = sort_for_size_and_complexity(Files),
  %hacking this in to convert the path to where the file will actually be.
  SortedFiles = lists:map(fun(File) -> file(false, File) end, SortedForComplexity),
  test_files(T, [SortedFiles|FilesAcc]).
test_files() ->
	FileGlobs = configuration:test_files_glob(),
	test_files(FileGlobs, []).

sort_for_size_and_complexity(Files) ->
	SortedOnSize = file_sort_on_size(Files),
	SlowFiles = potentially_slow_files(Files),
	SortedOnSizeMinusSlow = SortedOnSize -- SlowFiles,
	lists:append(SlowFiles, SortedOnSizeMinusSlow).
	
%%Specs like sould_behave_like should be ran first
potentially_slow_files(Files) ->
	potentially_slow_files(Files, []).
potentially_slow_files([], SlowFiles) -> 
  SlowFiles;
potentially_slow_files([File|Files], SlowFiles) ->
	{ok, ReadFile} = file:read_file(File),
	MatchData = re:run(ReadFile, ".*should_behave_like.*"),
	case MatchData of
		{match, _Pos} -> potentially_slow_files(Files, [File|SlowFiles]);
		nomatch -> potentially_slow_files(Files, SlowFiles)
	end.
	
file_sort_on_size(Files) ->
  SizeFun = fun(A,B) -> file_size(A) =< file_size(B) end,
  lists:reverse(lists:sort(SizeFun, Files)).
  
file_size(File) ->
  {ok, R} = file:read_file_info(File),
  #file_info{size=Size} = R,
  Size.

%checks if Runner is local and uses appropriate path
%TODO need to see if this needs to be optimized
%local
file(true, File) ->
	%TODO JUST RETURN FILE WHEN DONE WITH LOCAL TESTING File;
	file(false, File);
%remote
file(false, File) ->
	{ok, Dir} = file:get_cwd(),
	{ok, HostName} = inet:gethostname(),
	FileMinusLocalPath = File -- Dir,
	configuration:remote_dir() ++ HostName ++ "/" ++ project:project_name() ++ FileMinusLocalPath ;
file(Runner, File) ->
	%check if Runner is local to determine file path
	%TODO: I'm sure this is a horrible way of doing this
	file(lists:member(Runner, processes()), File).
	
open_dets_file() ->
  %TODO: Match open_file for issues
	{ok, Ref} = dets:open_file(file_time, [{file, configuration:remote_dir() ++ "file_time_dets"}]),
	Ref.

close_dets_file(Ref) ->
	dets:close(Ref).

log_file_time(FilesTime) ->
	Ref = open_dets_file(),
	log_file_time(FilesTime, Ref).
log_file_time([], DetsRef) ->
	close_dets_file(DetsRef);
log_file_time([H|FilesTime], DetsRef) ->
  dets:insert(DetsRef, H),
  log_file_time(FilesTime, DetsRef).

get_logged_file_time() ->
  Ref = open_dets_file(),
  %MatchSpec = ets:fun2ms(fun({File, Time}) -> {File, Time} end),
  MatchSpec = [{'$1',[],['$1']}],
  FilesTimes = dets:select(Ref, MatchSpec),
  close_dets_file(Ref),
  FilesTimes.

sort_time_fun() ->
  fun({_File,Time1}, {_File2, Time2}) ->
	  Time1 =< Time2 end.

sorted_files_per_historic_time() ->
	FilesTimes = get_logged_file_time(),
  Sorted = lists:reverse(lists:sort(sort_time_fun(), FilesTimes)),
  %Storing file name as binary in dets but reading in as a list from glob so
  %converting it to list
  [binary_to_list(File) || {File, _Time} <- Sorted].
