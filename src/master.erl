%% @author Curtis Carter <curtis@rubyhq.com>. 
%% @doc Handles the startup of all other processes
%% This is the central part of the application.  During a test run the primary responsibility
%% is to start the runners and then give them the files to be ran.<br/>
%% The vm that starts this will be halted when the tests are finished or the user kills it

-module(master).
-export([start/0]).
-vsn("0.0.3").

%% @doc Starts the logger, reporter, and node preparation processes
start() ->
	process_flag(trap_exit, true),
  start_logging(),
	register(master, self()),
	Reporter = reporter:start(),
	register(reporter, Reporter),
	%Once node is prepped it sends master process {node_ready, Host, RunnerCount}. This starts runners
  runner_node_prep:start(Reporter, self()),
  run_files(),
	self().
	
start_logging() ->
	LogFile =  "/tmp/distributest.log",
	error_logger:tty(false),
  error_logger:logfile({open, LogFile}),
	error_logger:info_msg("Starting test run").
	
stop_logging() ->
	error_logger:logfile(close).
			
run_files() ->
	FilesToRun = files:sorted_test_files(),
	loop(FilesToRun, [], []).

run_file(Runner, File) ->
	Runner ! {file, list_to_atom(File)}.

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
  stop_logging(),
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
	
runner_abnormal_down(remaining_runners, [], []) ->
  io:format("All runners died abnormally"),
  halt();
runner_abnormal_down(remaining_runners, Runners, RunnerRefs) -> {Runners, RunnerRefs}.
runner_abnormal_down(Ref, RunnerPid, Reason, Runners, RunnerRefs) ->
  io:format("~nRunner on ~p down: See log for more info~n ", [node(RunnerPid)] ),
  error_logger:error_msg("Runner Died~nHost: ~p~nPid: ~p~nReason: ~p", [node(RunnerPid), RunnerPid, Reason]),
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
		
		{'DOWN', Ref, process, Pid, normal} -> 
		  {Runners1, RunnerRefs1} = remove_pid_ref(Pid, Ref, Runners, RunnerRefs),
		  loop([FilesHead|FilesTail], Runners1, RunnerRefs1);
		
		%TODO: Tracking Refs for runners but not really using them
		{'DOWN', Ref, process, Pid, Reason} ->
			{RemainingRunners, RemainingRunnerRefs} = runner_abnormal_down(Ref, Pid, Reason, Runners, RunnerRefs),
			loop([FilesHead|FilesTail], RemainingRunners, RemainingRunnerRefs);

		Any ->
			io:format("DEBUG !!!!!!!!!!! Received:~p~n",[Any]),
			loop([], Runners, RunnerRefs)
	end.