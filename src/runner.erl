-module(runner).
-compile(export_all).

master_monitor(MasterNode) ->
	process_flag(trap_exit, true),
	erlang:monitor(process, MasterNode).

start(Node, MasterNode, RunnerNumber, Reporter, ProjectFilePath) ->
  spawn(Node, fun() -> setup_and_start(RunnerNumber, MasterNode, Reporter, ProjectFilePath) end).

setup_and_start(RunnerNumber, MasterNode, Reporter, ProjectFilePath) ->
  MasterMonitorReference = master_monitor(MasterNode),
  setup_environment(RunnerNumber, ProjectFilePath, MasterNode),
  startup_ruby(RunnerNumber, MasterMonitorReference, MasterNode, Reporter, ProjectFilePath).
  
%%Gets hostname from master and removes non alpha numeric characters from it.
%%Adds runner number to end
runner_identifier(RunnerNumber, MasterNode) ->
	MasterNode ! {master_hostname, self()},
	receive
		{master_hostname, Hostname} -> ok
	end,
	{match, NormalizedHostName} = re:run(Hostname,"[A-Za-z0-9]*",[global, notempty, {capture, all, list}]),
  lists:flatten(NormalizedHostName) ++ integer_to_list(RunnerNumber).

%%Note the ProjectFilePath is determined before the remote process is spawned. Going to change this
setup_environment(RunnerNumber, ProjectFilePath, MasterNode) ->
  SetupScript = "bash " ++ ProjectFilePath ++ "/spec/setup_environment.sh ",
  shell_command:run(ProjectFilePath, SetupScript ++ runner_identifier(RunnerNumber, MasterNode)).

startup_ruby(RunnerNumber, MasterMonitorReference, MasterNode, Reporter, ProjectFilePath) ->
	Cmd = "ruby " ++ ProjectFilePath ++ "/spec/distributest.rb " ++ runner_identifier(RunnerNumber, MasterNode),
  Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),

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
			  {no_results, Text} -> io:format("~n No results for file: ~p", [Text]);
			  {port_shutdown, _Text} -> stop()
			end,
	    MasterNode ! {ready_for_file, self()},
	    loop(X, Port, MasterNode, MasterMonitorReference, Reporter);
		 
	  %master sends this message to here causing the file to be ran
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