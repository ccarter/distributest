-module(master).
-compile(export_all).

start() ->
	ets:new(runners, [bag, named_table]),
	register(master, self()),
	Reporter = reporter:start(),
	register(reporter, Reporter),
  start_runners(Reporter),
  run_files(),
	self().

start_runners(Reporter) ->
	start_runners(configuration:runner_settings(), Reporter).
start_runners([], _) -> done;
start_runners([H|T], Reporter) -> 
  {{host, Host},{runner_count, RunnerCount}} = H,
	io:format("Host: ~p RunnerCount: ~p ~n", [Host, RunnerCount]),
	start_runner(Host, RunnerCount, Reporter),
	start_runners(T, Reporter).
  
start_runner(Host, 0, _) ->  
  done;
start_runner(Host, RunnerCount, Reporter) ->
	%local hostname to prefix remote node names
	{ok, LocalHostname} = inet:gethostname(),
	NodeName = list_to_atom(LocalHostname ++ "_runner" ++ "@" ++ Host),
	io:format("Starting runner number: ~p on host: ~p~n", [RunnerCount, Host]),
  RunnerPid = runner:start(NodeName, self(),  RunnerCount, Reporter),
  ets:insert(runners, {runner, RunnerPid}),
  start_runner(Host, RunnerCount - 1, Reporter).
	
run_files() ->
	FilesToRun = files:test_files(),
	loop(FilesToRun).

run_file(Runner, File) ->
	Runner ! {file, list_to_atom(File)}.

shutdown_runners([]) ->  
  io:format("~nDistributest done.~nHave a nice day.~n"),
  halt();
%TODO: maybee I should pass the runners around instead of getting them from ets
shutdown_runners(Runners) ->
  receive
	  {ready_for_file, Runner} ->
		%allow runner a moment to send any remaining messages to reporter
		  timer:sleep(3000),
		  %TODO: not needed anymore due to the way I'm recursing.
		  ets:delete_object(runners, {runner, Runner}),
		  exit(Runner, 'DONE'),
		  
		  shutdown_runners(Runners -- [Runner])
	end.

ets_runner_list() ->
  EtsRunnerList = ets:lookup(runners, runner),
  [Runner || {runner, Runner} <- EtsRunnerList]. 
	
loop([]) -> 
  shutdown_runners(ets_runner_list());
loop([FilesHead|FilesTail]) ->
	receive
    {ready_for_file, Runner} ->
	    run_file(Runner, FilesHead),
	    loop(FilesTail);

		Any ->
			io:format("DEBUG !!!!!!!!!!! Received:~p~n",[Any]),
			loop([])
	end.