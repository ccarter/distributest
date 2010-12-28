-module(runner).
-compile(export_all).

%%Node prep then starts the runner process
start_runners(Reporter, MasterPid) ->
	start_runners(configuration:runner_settings(), Reporter, MasterPid).
start_runners([], _, _) -> done;
start_runners([H|T], Reporter, MasterPid) -> 
  {{host, Host},{runner_count, RunnerCount}} = H,
  start_runners(Host, RunnerCount, T, Reporter, MasterPid).
	
%local host
start_runners("127.0.0.1", RunnerCount, RemainingHosts, Reporter, MasterPid) ->
  io:format("Host: Local RunnerCount: ~p ~n", [RunnerCount]),
%TODO remove once rsync testing locally is done
  spawn(fun() -> sync_files("127.0.0.1", RunnerCount, Reporter, MasterPid) end),
	start_runners(RemainingHosts, Reporter, MasterPid);
%disabled host as RunnerCount is 0
start_runners(Host, 0, RemainingHosts, Reporter, MasterPid) ->
	io:format("Host: ~p is set to 0 runners ~n", [Host]),
	start_runners(RemainingHosts, Reporter, MasterPid);
%remote host
%%TODO MOVE ALL THIS SETUP STUFF TO DIFF MODULE. Currently spawning a runner process locally
%%to setup and then it spawns the remote runner process on nodes. This module should only be responsible
%%for 1 thing
start_runners(Host, RunnerCount, RemainingHosts, Reporter, MasterPid) ->
	io:format("Host: ~p RunnerCount: ~p ~n", [Host, RunnerCount]),
  spawn(fun() -> sync_files(Host, RunnerCount, Reporter, MasterPid) end),
	start_runners(RemainingHosts, Reporter, MasterPid).
 
%TODO: See if I can move this down into start_runner
sync_files(Host, RunnerCount, Reporter, MasterPid) ->
	shell_command:rsync_local(),
  start_runner(Host, RunnerCount, Reporter, MasterPid).

start_runner(_,0,_,_) ->  
  done;
start_runner(Host, RunnerCount, Reporter, MasterPid) ->
	%local hostname to prefix remote node names
	{ok, LocalHostname} = inet:gethostname(),
	NodeName = list_to_atom(LocalHostname ++ "_runner" ++ "@" ++ Host),
	io:format("Starting runner number: ~p on host: ~p~n", [RunnerCount, Host]),
  RunnerPid = runner:start(NodeName, MasterPid,  RunnerCount, Reporter, project:remote_path()),
  ets:insert(runners, {runner, RunnerPid}),
  start_runner(Host, RunnerCount - 1, Reporter, MasterPid).

%%END NODE PREP

start(Node, MasterNode, RunnerNumber, Reporter, ProjectFilePath) ->
  spawn(Node, fun() -> setup_and_start(RunnerNumber, MasterNode, Reporter, ProjectFilePath) end).

setup_and_start(RunnerNumber, MasterNode, Reporter, ProjectFilePath) ->
  setup_environment(RunnerNumber, ProjectFilePath),
  startup_ruby(RunnerNumber, MasterNode, Reporter, ProjectFilePath).
  
runner_identifier(RunnerNumber) ->
  integer_to_list(RunnerNumber).

%%Note the ProjectFilePath is determined before the remote process is spawned
%%TODO figure out where to move the setup script
setup_environment(RunnerNumber, ProjectFilePath) ->
  SetupScript = "bash /Users/racker/erlang/configuration/setup_environment.sh ",
  shell_command:run(ProjectFilePath, SetupScript ++ runner_identifier(RunnerNumber)).

startup_ruby(RunnerNumber, MasterNode, Reporter, ProjectFilePath) ->
	Cmd = "ruby " ++ ProjectFilePath ++ "/curtis_spec.rb " ++ runner_identifier(RunnerNumber),
  Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),

  %tell the master we are ready to start running files
  MasterNode ! {ready_for_file, self()},
  loop([], Port, MasterNode, Reporter).

loop(X, Port, MasterNode, Reporter) ->
	receive
		%receives the results from the ruby process
		{Port, {data, Data}} ->			
			case binary_to_term(Data) of
		    {pass_results, Text} -> Reporter ! {pass_results, Text};
			  {fail_results, Text} -> Reporter ! {fail_results, Text};
			  {no_results, Text} -> io:format("~n No results for file: ~p", [Text])
			end,
	    MasterNode ! {ready_for_file, self()},
	    loop(X, Port, MasterNode, Reporter);
		 
	  %master sends this message to here causing the file to be ran
	  %TODO relook at port_command vs bang
	  {file, File} ->
		  Payload = term_to_binary({file, atom_to_binary(File, latin1)}),
		  port_command(Port, Payload),
		  loop(X, Port, MasterNode, Reporter);
		  
		%grab everything that doesn't match. FOR DEVELOPMENT DEBUGING
		Any ->
			io:format("Received:~p~n",[Any]),
			loop(X, Port, MasterNode, Reporter)
	end.