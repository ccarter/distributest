-module(runner_node_prep).
-export([start/2]).

%%Node prep then starts the runner process
start(Reporter, MasterPid) ->
	start(configuration:runner_settings(), Reporter, MasterPid).
start([], _, _) -> done;
start([H|T], Reporter, MasterPid) -> 
  {{host, Host},{runner_count, RunnerCount}} = H,
  start(Host, RunnerCount, T, Reporter, MasterPid).
	
%local host
start("127.0.0.1", RunnerCount, RemainingHosts, Reporter, MasterPid) ->
  io:format("Host: Local RunnerCount: ~p ~n", [RunnerCount]),
%TODO remove once rsync testing locally is done
  spawn(fun() -> sync_files("127.0.0.1", RunnerCount, Reporter, MasterPid) end),
	start(RemainingHosts, Reporter, MasterPid);
%disabled host as RunnerCount is 0
start(Host, 0, RemainingHosts, Reporter, MasterPid) ->
	io:format("Host: ~p is set to 0 runners ~n", [Host]),
	start(RemainingHosts, Reporter, MasterPid);
%remote host
%%TODO MOVE ALL THIS SETUP STUFF TO DIFF MODULE. Currently spawning a runner process locally
%%to setup and then it spawns the remote runner process on nodes.
%%Also remove the runner process ProjectFilePath 's from being passed in and around
start(Host, RunnerCount, RemainingHosts, Reporter, MasterPid) ->
	io:format("Host: ~p RunnerCount: ~p ~n", [Host, RunnerCount]),
  spawn(fun() -> sync_files(Host, RunnerCount, Reporter, MasterPid) end),
	start(RemainingHosts, Reporter, MasterPid).
 
%TODO: See if I can move this down into start_runner
sync_files(Host, RunnerCount, Reporter, MasterPid) ->
	shell_command:rsync_local(),
  start_runner(Host, RunnerCount, Reporter, MasterPid).

start_runner(_,0,_,_) ->  
  done;
start_runner(Host, RunnerCount, Reporter, MasterPid) ->
	NodeName = list_to_atom("runner" ++ "@" ++ Host),
	io:format("Starting runner number: ~p on host: ~p~n", [RunnerCount, Host]),
  RunnerPid = runner:start(NodeName, MasterPid,  RunnerCount, Reporter, project:remote_path()), 
  MasterPid ! {runner_up, RunnerPid},  

  start_runner(Host, RunnerCount - 1, Reporter, MasterPid).

%%END NODE PREP

