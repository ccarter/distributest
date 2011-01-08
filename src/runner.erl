-module(runner).
-export([start_runners/4]).

-define(RUNNER_SETUP_FILE, "distributest/runner_setup").
-define(DISTRIBUTEST_RUBY_FILE, "distributest/distributest.rb").

master_monitor(MasterNode) ->
	process_flag(trap_exit, true),
	erlang:monitor(process, MasterNode).
	
start_runners(Host, RunnerCount, Reporter, MasterPid) ->
	start_runners(Host, RunnerCount, Reporter, MasterPid, [], []).
start_runners(_,0,_,_, Runners, RunnerMonitorRefs) ->  
  {Runners, RunnerMonitorRefs};
start_runners(Host, RunnerCount, Reporter, MasterPid, Runners, RunnerMonitorRefs) ->
	NodeName = runner_node_prep:node_name(Host),
	io:format("Starting runner number: ~p on host: ~p~n", [RunnerCount, Host]),
  {Runner, RunnerMonitorRef} = start(NodeName, MasterPid,  RunnerCount, Reporter, project:remote_path()), 
  start_runners(Host, RunnerCount - 1, Reporter, MasterPid, [Runner|Runners],[RunnerMonitorRef|RunnerMonitorRefs]).

start(Node, MasterNode, RunnerNumber, Reporter, ProjectFilePath) ->
  Runner = spawn(Node, fun() -> setup_and_start(RunnerNumber, MasterNode, Reporter, ProjectFilePath) end),
  %monitor from master
  Ref = erlang:monitor(process, Runner),
  {Runner, Ref}.

setup_and_start(RunnerNumber, MasterNode, Reporter, ProjectFilePath) ->
  MasterMonitorReference = master_monitor(MasterNode),
  RunnerIdentifier = runner_identifier(RunnerNumber, MasterNode),
  setup_environment(RunnerIdentifier, ProjectFilePath),
  startup_ruby(RunnerIdentifier, MasterMonitorReference, MasterNode, Reporter, ProjectFilePath).
  
%%Gets hostname from master and removes non alpha numeric characters from it.
%%Adds runner number to end
runner_identifier(RunnerNumber, MasterNode) ->
	MasterNode ! {master_hostname, self()},
	Hostname = receive
		{master_hostname, Name} -> Name
		after 7000 ->
			%TODO Log this in file
			exit("MasterHostnameFailure")
	end,
	{match, NormalizedHostName} = re:run(Hostname,"[A-Za-z0-9]*",[global, notempty, {capture, all, list}]),
  lists:flatten(NormalizedHostName) ++ integer_to_list(RunnerNumber).

%%Note the ProjectFilePath is determined before the remote process is spawned. Going to change this
setup_environment(RunnerIdentifier, ProjectFilePath) ->
  SetupScript = "bash " ++ ProjectFilePath ++ "/" ++ ?RUNNER_SETUP_FILE ++ " ",
  shell_command:run(ProjectFilePath, SetupScript ++ RunnerIdentifier).

startup_ruby(RunnerIdentifier, MasterMonitorReference, MasterNode, Reporter, ProjectFilePath) ->
	Cmd = "ruby " ++ ProjectFilePath ++ "/" ++ ?DISTRIBUTEST_RUBY_FILE ++ " " ++ RunnerIdentifier,
  Port = open_port({spawn, Cmd}, [{packet, 4}, {cd, ProjectFilePath}, nouse_stdio, exit_status, binary]),

  %tell the master we are ready to start running files
  MasterNode ! {ready_for_file, self()},
  loop([], Port, MasterNode, MasterMonitorReference, Reporter).
	
loop(X, Port, MasterNode, MasterMonitorReference, Reporter) ->
	receive
		%receives the results from the ruby process
		{Port, {data, Data}} ->			
			case binary_to_term(Data) of
		    {pass_results, Text} -> Reporter ! {pass_results, Text};
			  {fail_results, Text} -> Reporter ! {fail_results, Text};
			  {total_time_for_file, File, Time} -> Reporter ! {total_time_for_file, File, Time};
			  {no_results, Text} -> io:format("~n No results for file: ~p", [Text]);
			  ready_for_file -> MasterNode ! {ready_for_file, self()};
			  {port_shutdown, _Text} -> stop()
			end,
	    loop(X, Port, MasterNode, MasterMonitorReference, Reporter);
		 
	  %runs this file
	  %TODO relook at port_command vs bang
	  {file, File} ->
		  Payload = term_to_binary({file, atom_to_binary(File, latin1)}),
		  port_command(Port, Payload),
		  loop(X, Port, MasterNode, MasterMonitorReference, Reporter);
		 
		%Master kills runner process this way when it's successfully completed
		{'EXIT', _, 'DONE'} -> 
		  stop_port(Port),
		  loop(X, Port, MasterNode, MasterMonitorReference, Reporter);
		
		%catches message from port processes during runner setup
		{'EXIT', _Pid, normal} -> loop(X, Port, MasterNode, MasterMonitorReference, Reporter);
		
		%if the master is killed we use this to give the runner a clean exit
		{'DOWN', MasterMonitorReference, process, _Pid, _Reason} -> 
      stop_port(Port),
		  loop(X, Port, MasterNode, MasterMonitorReference, Reporter);
		
		%Grabbing ruby exits here and stopping the runner
		{Port, {exit_status, _ExitNumber}} -> exit(rubydied);

		%grab everything that doesn't match. FOR DEVELOPMENT DEBUGING
		Any ->
			io:format("Received:~p~n",[Any]),
			loop(X, Port, MasterNode, MasterMonitorReference, Reporter)
	end.
	
stop_port(Port) ->
  port_command(Port, term_to_binary('stop')).
	
stop() ->
	exit(normal).