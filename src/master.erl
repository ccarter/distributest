-module(master).
-export([start/0]).
-vsn("0.0.3").

start() ->
	process_flag(trap_exit, true),
	register(master, self()),
	Reporter = reporter:start(),
	register(reporter, Reporter),
	%Once node is prepped it sends master process {node_ready, Host, RunnerCount}. This starts runners
  runner_node_prep:start(Reporter, self()),
  run_files(),
	self().
	
run_files() ->
	FilesToRun = files:test_files(),
	loop(FilesToRun, [], []).

run_file(Runner, File) ->
	FileWithCorrectPath = files:file(Runner, File),
	Runner ! {file, list_to_atom(FileWithCorrectPath)}.

start_runners(Host, RunnerCount) ->	
  runner:start_runners(Host, RunnerCount, whereis(reporter), self()).

shutdown_runners([]) ->
	reporter ! {shutdown, self()},
	receive
		reporter_down -> ok
	after 10000 ->
		io:format("Timeout waiting for reporter to shutdown")
	end,
  io:format("~nDistributest done.~nHave a nice day.~n"),
  halt();

shutdown_runners(Runners) ->
  receive
	  {ready_for_file, Runner} ->
		  runner_stop(Runner),		  
		  shutdown_runners(Runners -- [Runner]);
		%TODO: bug that can occur if all files are being ran before a runner is setup. This is temp fix
		{master_hostname, Runner} -> 
		  runner_stop(Runner),
		  exit(Runner, kill),
		  shutdown_runners(Runners -- [Runner])
	end.

master_hostname() -> 
	{ok, Hostname} = inet:gethostname(),
	Hostname.

runner_stop(RunnerPid) ->
	exit(RunnerPid, 'DONE').
	
%%TODO log abnormal runner shutdowns
runner_abnormal_down(remaining_runners, [], []) ->
  io:format("All runners died abnormally"),
  halt();
runner_abnormal_down(remaining_runners, Runners, RunnerRefs) -> {Runners, RunnerRefs}.
runner_abnormal_down(Ref, RunnerPid, Runners, RunnerRefs) ->
  io:format("~nRUNNER ABNORMAL DOWN~p~n ", [RunnerPid] ),
  {RemainingRunners, RemainingRunnerRefs} = remove_pid_ref(RunnerPid, Ref, Runners, RunnerRefs),
	runner_abnormal_down(remaining_runners, RemainingRunners, RemainingRunnerRefs).
	
remove_pid_ref(Pid, Ref, Pids, Refs) ->
	{Pids -- [Pid], Refs -- [Ref]}.

add_pid_ref(NewPids, NewRefs, Pids, Refs) ->
  AppendedRefsList = lists:append(NewRefs, Refs),
	AppendedPidsList = lists:append(NewPids, Pids),
	{AppendedPidsList, AppendedRefsList}.
		
loop([], Runners, _RunnerRefs) -> 
  shutdown_runners(Runners);
loop([FilesHead|FilesTail], Runners, RunnerRefs) ->
	receive
		{node_ready, Host, RunnerCount} ->
  		{NewRunners, NewRunnerRefs} = start_runners(Host, RunnerCount),
      {AppendedRunnerList, AppendedRefsList} = add_pid_ref(NewRunners, NewRunnerRefs, Runners, RunnerRefs),
			loop([FilesHead|FilesTail], AppendedRunnerList, AppendedRefsList);
						
    {ready_for_file, Runner} ->
	    run_file(Runner, FilesHead),
	    loop(FilesTail, Runners, RunnerRefs);
	
	  {master_hostname, RunnerPid} ->
		  RunnerPid ! {master_hostname, master_hostname()},
		  loop([FilesHead|FilesTail], Runners, RunnerRefs);
		
		%Not really needed, but here incase a normal goes down normally before end of run
		{'DOWN', Ref, process, Pid, normal} -> 
		  {Runners1, RunnerRefs1} = remove_pid_ref(Pid, Ref, Runners, RunnerRefs),
		  loop([FilesHead|FilesTail], Runners1, RunnerRefs1);
		
		%TODO: Tracking Refs for runners but not really using them
		{'DOWN', Ref, process, Pid, _Reason} ->
			{RemainingRunners, RemainingRunnerRefs} = runner_abnormal_down(Ref, Pid, Runners, RunnerRefs),
			loop([FilesHead|FilesTail], RemainingRunners, RemainingRunnerRefs);

		Any ->
			io:format("DEBUG !!!!!!!!!!! Received:~p~n",[Any]),
			loop([], Runners, RunnerRefs)
	end.