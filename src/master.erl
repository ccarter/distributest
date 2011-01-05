-module(master).
-export([start/0]).

start() ->
	process_flag(trap_exit, true),
  ets:new(runners, [bag, public, named_table]),
	register(master, self()),
	Reporter = reporter:start(),
	register(reporter, Reporter),
  runner_node_prep:start(Reporter, self()),
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
		%TODO: Had some timing issues on shutdown, not sure if still needed
%		  timer:sleep(3000),
		  runner_stop(Runner),
%		  ets:delete_object(runners, {runner, Runner}),
%		  exit(Runner, 'DONE'),
		  
		  shutdown_runners(Runners -- [Runner]);
		%TODO: bug that can occur if all files are being ran before a runner is setup. This is temp fix
		{master_hostname, Runner} -> 
		  runner_stop(Runner),
		  exit(Runner, kill),
		  shutdown_runners(Runners -- [Runner])
	end.

ets_runner_list() ->
  EtsRunnerList = ets:lookup(runners, runner),
  [Runner || {runner, Runner} <- EtsRunnerList]. 

master_hostname() -> 
	{ok, Hostname} = inet:gethostname(),
	Hostname.

runner_stop(RunnerPid) ->
	ets:delete_object(runners, {runner, RunnerPid}),
	exit(RunnerPid, 'DONE').
	
%%Due to the multi process and async way I am starting runners I send a message back to master 
%%when a runner is spawned.Currently only used to setup tracking
%%TODO Not atomic.Need to redesign.Process is monitoring master as well so chance of hung Runners is small
runner_up(RunnerPid) ->
	ets:insert(runners, {runner, RunnerPid}), 
	erlang:monitor(process, RunnerPid).
	
%%TODO log abnormal runner shutdowns
%%Might want to rethink the halt as the first runner could die before the 2nd one is going
runner_abnormal_down(check_if_any_left) ->
	case ets_runner_list() == [] of
		false -> ok;
		true -> 
		  io:format("All runners died abnormally"),
		  halt()
	end;
runner_abnormal_down(RunnerPid) ->
	ets:delete_object(runners, {runner, RunnerPid}),
	runner_abnormal_down(check_if_any_left).
	
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
		
		{runner_up, RunnerPid} ->
			runner_up(RunnerPid),
			loop([FilesHead|FilesTail]);
			
		%For runner going down, may need to relook at better way if I ever monitor other types of processes
		{'DOWN', _Ref, process, Pid, _Reason} ->
			runner_abnormal_down(Pid),
			loop([FilesHead|FilesTail]);

		Any ->
			io:format("DEBUG !!!!!!!!!!! Received:~p~n",[Any]),
			loop([])
	end.