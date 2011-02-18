%% @author Curtis Carter <curtis@rubyhq.com>. 
%% @doc Handles the Ruby to Erlang communication and forwards messages to master and reporter process<br/>
%% Multiple processes using this module will be on each node <br/>
%% The version of this module is tied to the version of the Distributest gem.
%% The runner process using this module also handles setting up the runner before going into it's loop ie. runner_setup script

-module(runner).
-export([start_runners/4, version/0]).
-vsn("0.0.5").

-define(GLOBAL_RUNNER_SETUP_FILE, "/runner_setup").
-define(PROJECTS_RUNNER_SETUP_FILE, "distributest/runner_setup").
-define(DISTRIBUTEST_RUBY_FILE, "distributest/distributest_runner.rb").

master_monitor(MasterNode) ->
	process_flag(trap_exit, true),
	erlang:monitor(process, MasterNode).
	
%% @spec start_runners(Host, RunnerCount, Reporter, MasterPid) -> {Runners, RunnerMonitorRefs}
%% where Host = string()
%%       RunnerCount = integer()
%%       Reporter = pid()
%%       MasterPid = pid()
%%       Runners = [pid()]
%%       RunnerMonitorRefs = [reference()]
%%
%% @doc Spaws runner process * RunnerCount
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
	%Need to get this before remote spawn because of hostname used in path
	GlobalSetupFile = project:remote_global_setup_script_path() ++ ?GLOBAL_RUNNER_SETUP_FILE,
  Runner = spawn(Node, fun() -> setup_and_start(RunnerNumber, MasterNode, Reporter, ProjectFilePath, GlobalSetupFile) end),
  %monitor from master
  Ref = erlang:monitor(process, Runner),
  {Runner, Ref}.

setup_and_start(RunnerNumber, MasterNode, Reporter, ProjectFilePath, RunnerSetupFile1) ->
  MasterMonitorReference = master_monitor(MasterNode),
  RunnerIdentifier = runner_identifier(RunnerNumber, MasterNode),
  setup_environment(RunnerIdentifier, ProjectFilePath, MasterNode, RunnerSetupFile1),
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
%%This runs the Ruby projects runner_setup script if it exists.If it doesn't it tries to run one that was
%%copied from the system initiating the tests. Neither file is required
setup_environment(RunnerIdentifier, ProjectFilePath, MasterNode, GlobalSetupFile) ->
	MasterMonitorRef = master_monitor(MasterNode),
	ProjectSetupFile = ProjectFilePath ++ "/" ++ ?PROJECTS_RUNNER_SETUP_FILE,
  case shell_command:run_file_if_exists_with_monitor(ProjectFilePath, ProjectSetupFile, RunnerIdentifier, {monitor, MasterMonitorRef}, {identifier, RunnerIdentifier}) of
  	{error, enoent} ->   
      shell_command:run_file_if_exists_with_monitor(ProjectFilePath, GlobalSetupFile, RunnerIdentifier, {monitor, MasterMonitorRef}, {identifier, RunnerIdentifier});
    _Any -> ok
  end.

startup_ruby(RunnerIdentifier, MasterMonitorReference, MasterNode, Reporter, ProjectFilePath) ->
	Cmd = "ruby -e \"require 'rubygems';gem 'distributest', '= " ++ version() ++ "';require 'distributest'; Distributest.start('" ++ RunnerIdentifier ++ "')\"",
  Port = open_port({spawn, Cmd}, [{packet, 4}, {cd, ProjectFilePath}, nouse_stdio, exit_status, binary]),

  %tell the master we are ready to start running files
  MasterNode ! {ready_for_file, self()},
  loop(Port, MasterNode, MasterMonitorReference, Reporter, RunnerIdentifier).
	
loop(Port, MasterNode, MasterMonitorReference, Reporter, RunnerIdentifier) ->
	receive
		%receives the results from the ruby process
		{Port, {data, Data}} ->			
			case binary_to_term(Data) of
		    {pass_results, Text} -> Reporter ! {pass_results, Text};
			  {fail_results, Text} -> Reporter ! {fail_results, Text};
			  {total_time_for_file, File, Time} -> Reporter ! {total_time_for_file, File, Time};
			  {profile, File, Profile} -> Reporter ! {profile, File, Profile};
			  {no_results, Text} -> io:format("~n No results for file: ~p", [Text]);
			  ready_for_file -> MasterNode ! {ready_for_file, self()};
			  {captured_std_err_out, Text} -> Reporter ! {captured_std_err_out, Text, self()};
			  {port_shutdown, _Text} -> stop()
			end,
	    loop(Port, MasterNode, MasterMonitorReference, Reporter, RunnerIdentifier);
		 
	  %runs this file
	  %TODO relook at port_command vs bang
	  {file, File} ->
		  Payload = term_to_binary({file, atom_to_binary(File, latin1)}),
		  port_command(Port, Payload),
		  loop(Port, MasterNode, MasterMonitorReference, Reporter, RunnerIdentifier);
		 
		%Master kills runner process this way when it's successfully completed
		{'EXIT', _, 'DONE'} -> 
		  stop_port(Port),
		  loop(Port, MasterNode, MasterMonitorReference, Reporter, RunnerIdentifier);
		
		%catches message from port processes during runner setup
		{'EXIT', _Pid, normal} -> 
		  loop(Port, MasterNode, MasterMonitorReference, Reporter, RunnerIdentifier);
		
		%if the master is killed we force kill the ruby process and exit
		{'DOWN', MasterMonitorReference, process, _Pid, _Reason} -> 
  		Port ! {self(), close},
      shell_command:kill_port_process(RunnerIdentifier),
      exit(master_down);
		
		%Grabbing ruby exits here and stopping the runner
		{Port, {exit_status, _ExitNumber}} -> exit(rubydied);

		%grab everything that doesn't match. FOR DEVELOPMENT DEBUGING
		Any ->
			io:format("Received:~p~n",[Any]),
			loop(Port, MasterNode, MasterMonitorReference, Reporter, RunnerIdentifier)
	end.

%% @doc returns current version of this module <br/>
%% Used to determine what version of Distributest gem to load
version() ->
	{vsn, Version} = lists:keyfind(vsn, 1, ?MODULE:module_info(attributes)),
	Version.
	
stop_port(Port) ->
  port_command(Port, term_to_binary('stop')).
	
stop() ->
	exit(normal).