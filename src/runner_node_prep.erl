-module(runner_node_prep).
-export([start/2, node_name/1]).
-vsn("0.0.3").

-include("includes/configuration.hrl").

-define(RSYNC_USER, "racker").
-define(NODE_SETUP_FILE1, "/node_setup").
-define(NODE_SETUP_FILE2, "distributest/node_setup").

%%% THIS DOES NOT KILL -9 THE NODE_PREP SCRIPT ON THE NODES LIKE RUNNER DOES
%%% This is because this should not be setting up dbs or anything that can't just finish
%%% Will probably change in the future

start(Reporter, MasterPid) ->
	start(configuration:runner_settings(), Reporter, MasterPid).
start([], _, _) -> done;
start([H|T], Reporter, MasterPid) -> 
   start(H#node_settings.host, H#node_settings.runner_count, H#node_settings.ssh_user, T, Reporter, MasterPid).
  
%%disabled host as RunnerCount is 0
start(Host, 0, _SshUser, RemainingHosts, Reporter, MasterPid) ->
	io:format("Host: ~p is set to 0 runners ~n", [Host]),
	start(RemainingHosts, Reporter, MasterPid);

%%Currently spawning a runner process locally
%%to setup and then it spawns the remote runner process on nodes.
start(Host, RunnerCount, SshUser, RemainingHosts, Reporter, MasterPid) ->
	io:format("Host: ~p RunnerCount: ~p ~n", [Host, RunnerCount]),
  spawn(fun() -> node_prep(Host, RunnerCount, SshUser, MasterPid) end),
	start(RemainingHosts, Reporter, MasterPid).

node_prep(Host, RunnerCount, SshUser, MasterPid) ->
	case runner_vm_status(Host) of
		up -> good;
		down -> exit(normal)
	end,
	sync_files(Host, SshUser),
	RemotePath = project:remote_path(),
	GlobalSetupScriptPath = project:remote_global_setup_script_path(),
	MasterNodePrepPid = self(),
	spawn_link(node_name(Host), fun() -> node_prep_script(RemotePath, GlobalSetupScriptPath, MasterNodePrepPid) end),
	receive
		done_with_prep_script -> ok
	after 190000 ->
		io:format("Time out running node prep script"),
		exit(timeoutnodeprepscript)
	end,
	node_ready(Host, RunnerCount, MasterPid).
 
sync_files(Host, SshUser) ->
	case node_local(Host) of
		true -> sync_files(local, Host, SshUser);
		false -> sync_files(remote, Host, SshUser)
	end.
sync_files(local, _Host, _SshUser) ->
	shell_command:rsync_local();
sync_files(remote, Host, SshUser) ->
	UserHost = SshUser ++ "@" ++ Host,
	shell_command:rsync_remote(UserHost).
	
%%TODO look at error handling between local and remote node setup
node_prep_script(ProjectFilePath, GlobalSetupScriptPath, MasterPrepProcess) ->
	%first run the node prep thats in the installation if exists.
	%This is rsynced to remote nodes into project:global_setup_script_path() before being ran
	shell_command:run_file_if_exists(ProjectFilePath, GlobalSetupScriptPath ++ ?NODE_SETUP_FILE1),
	%Run the node prep thats in the Ruby project if it exists: project/distributest/node_prep
	File2 = ProjectFilePath ++ "/" ++ ?NODE_SETUP_FILE2,
  shell_command:run_file_if_exists(ProjectFilePath, File2),
  MasterPrepProcess ! done_with_prep_script.
  
node_ready(Host, RunnerCount, MasterPid) ->
	MasterPid ! {node_ready, Host, RunnerCount}.

node_name(Host) ->
	list_to_atom("runner" ++ "@" ++ Host).

%%TODO This is forcing fully qualified hostnames to be provided in the node_config	
node_local(Host) ->
	{ok, Hostname} = inet:gethostname(),
	TokenHost = string:tokens(Host, "."),
	[H|_T] = TokenHost,
	H == Hostname.
	
%% Checked on each node when tests are started
%% This does not preclude the tests from running on other nodes
runner_vm_status(Host) ->
	NodeName = node_name(Host),
	case net_adm:ping(NodeName) of
	  pong -> up;
		pang ->
		  log_runner_vm_down(NodeName), 
	    down
  end.

log_runner_vm_down(NodeName) ->
	io:format("**************************~n"
	          "***Runner VM ~p is down~n"
	          "***Check that your node_config.txt fully matches the name on the runner vm and that it's running~n"
	          "**************************~n"
	          ,[NodeName]),
	error_logger:error_msg("Runner VM ~p is down~n"
	                       "NOTE: If this was the only runner your tests aren't running", [NodeName]).