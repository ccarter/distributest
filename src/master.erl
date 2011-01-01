-module(master).
-compile(export_all).

start() ->
  ets:new(runners, [bag, public, named_table]),
	register(master, self()),
	Reporter = reporter:start(),
	register(reporter, Reporter),
  runner:start_runners(Reporter, self()),
  run_files(),
	self().
	
run_files() ->
	FilesToRun = files:test_files(),
	loop(FilesToRun).

run_file(Runner, File) ->
	FileWithCorrectPath = files:file(Runner, File),
	Runner ! {file, list_to_atom(FileWithCorrectPath)}.

shutdown_runners([]) ->  
  io:format("~nDistributest done.~nHave a nice day.~n"),
  halt();
%TODO: maybee I should pass the runners around instead of getting them from ets
shutdown_runners(Runners) ->
  receive
	  {ready_for_file, Runner} ->
		%allow runner a moment to send any remaining messages to reporter
		  timer:sleep(3000),
		  %TODO: not really needed anymore
		  ets:delete_object(runners, {runner, Runner}),
		  exit(Runner, 'DONE'),
		  
		  shutdown_runners(Runners -- [Runner])
	end.

ets_runner_list() ->
  EtsRunnerList = ets:lookup(runners, runner),
  [Runner || {runner, Runner} <- EtsRunnerList]. 

master_hostname() -> 
	{ok, Hostname} = inet:gethostname(),
	Hostname.
	
loop([]) -> 
  shutdown_runners(ets_runner_list());
loop([FilesHead|FilesTail]) ->
	receive
    {ready_for_file, Runner} ->
	    run_file(Runner, FilesHead),
	    loop(FilesTail);
	
	  {master_hostname, RunnerPid} ->
		  RunnerPid ! {master_hostname, master_hostname()},
		  loop([FilesHead|FilesTail]);

		Any ->
			io:format("DEBUG !!!!!!!!!!! Received:~p~n",[Any]),
			loop([])
	end.