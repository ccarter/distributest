-module(runner).
-compile(export_all).

start(Node, MasterNode, RunnerNumber, Reporter) ->
  spawn(Node, fun() -> setup_and_start(RunnerNumber, MasterNode, Reporter) end).

setup_and_start(RunnerNumber, MasterNode, Reporter) ->
  setup_environment(RunnerNumber),
  startup_ruby(RunnerNumber, MasterNode, Reporter).
  
runner_identifier(RunnerNumber) ->
  integer_to_list(RunnerNumber).

setup_environment(RunnerNumber) ->
  SetupScript = "bash /Users/racker/erlang/configuration/setup_environment.sh ",
  DirToRunIn = "/Users/racker/Projects/ip_commander",
  shell_command:run(DirToRunIn, SetupScript ++ runner_identifier(RunnerNumber)).

startup_ruby(RunnerNumber, MasterNode, Reporter) ->
	Cmd = "ruby /Users/racker/Projects/ip_commander/curtis_spec.rb " ++ integer_to_list(RunnerNumber),
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