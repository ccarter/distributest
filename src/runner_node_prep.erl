-module(runner_node_prep).
-export([start/2, node_name/1]).
-define(RSYNC_USER, "racker").
-define(NODE_SETUP_FILE, "distributest/node_setup").

%%Node prep then starts the runner process(s)
start(Reporter, MasterPid) ->
	start(configuration:runner_settings(), Reporter, MasterPid).
start([], _, _) -> done;
start([H|T], Reporter, MasterPid) -> 
  {{host, Host},{runner_count, RunnerCount}} = H,
  start(Host, RunnerCount, T, Reporter, MasterPid).
	
%%local host
start("127.0.0.1", RunnerCount, RemainingHosts, Reporter, MasterPid) ->
  io:format("Host: Local RunnerCount: ~p ~n", [RunnerCount]),
  %TODO remove once rsync testing locally is done
  spawn_monitor(fun() -> node_prep("127.0.0.1", RunnerCount, MasterPid) end),
	start(RemainingHosts, Reporter, MasterPid);
%%disabled host as RunnerCount is 0
start(Host, 0, RemainingHosts, Reporter, MasterPid) ->
	io:format("Host: ~p is set to 0 runners ~n", [Host]),
	start(RemainingHosts, Reporter, MasterPid);
%%remote host
%%Currently spawning a runner process locally
%%to setup and then it spawns the remote runner process on nodes.
start(Host, RunnerCount, RemainingHosts, Reporter, MasterPid) ->
	io:format("Host: ~p RunnerCount: ~p ~n", [Host, RunnerCount]),
  %spawn(fun() -> sync_files(Host, RunnerCount, Reporter, MasterPid) end),
  spawn(fun() -> node_prep(Host, RunnerCount, MasterPid) end),
	start(RemainingHosts, Reporter, MasterPid).
	
node_prep(Host, RunnerCount, MasterPid) ->
	sync_files(Host),
	RemotePath = project:remote_path(),
	MasterNodePrepPid = self(),
	spawn(node_name(Host), fun() -> node_prep_script(RemotePath, MasterNodePrepPid) end),
	receive
		done_with_prep_script -> ok
	after 120000 ->
		io:format("Time out running node prep script"),
		exit(timeoutnodeprepscrip)
	end,
	node_ready(Host, RunnerCount, MasterPid).
 
sync_files("127.0.0.1") ->
	shell_command:rsync_local();
sync_files(Host) ->
	UserHost = ?RSYNC_USER ++ "@" ++ Host,
	shell_command:rsync_remote(UserHost).
	
%%TODO look at error handling between local and remote node setup
node_prep_script(ProjectFilePath, MasterPrepProcess) ->
	SetupScript = "bash " ++ ProjectFilePath ++ "/" ++ ?NODE_SETUP_FILE,
  shell_command:run(ProjectFilePath, SetupScript),
  MasterPrepProcess ! done_with_prep_script.
 
node_ready(Host, RunnerCount, MasterPid) ->
	MasterPid ! {node_ready, Host, RunnerCount}.

node_name(Host) ->
	list_to_atom("runner" ++ "@" ++ Host).

