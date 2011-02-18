%% @author Curtis Carter <curtis@rubyhq.com>. 
%% @doc Prepares all the nodes participating in the distributed test before the runners are spawned<br/>
%% There are 1 process per node spawned on the system initiating the tests. 
%% This rsycs all the files to each node. Then a process is spawned on each remote node and they run the node_setup script in the project directory

-module(runner_node_prep).
-export([start/2, node_name/1]).
-vsn("0.0.4").

-include("includes/configuration.hrl").

-define(RSYNC_USER, "racker").
-define(GLOBAL_NODE_SETUP_FILE, "/node_setup").
-define(PROJECTS_NODE_SETUP_FILE, ".distributest/node_setup").

%% THIS DOES NOT KILL -9 THE NODE_PREP SCRIPT ON THE NODES LIKE RUNNER DOES
%% Will probably change in the future

%% @todo Not sure what this returns
%% @spec start(Reporter, MasterPid) -> nil()
%% where Reporter = pid()
%%       MasterPid = pid()
%%
%% @doc Looks up the configuration for a node and spawns a local process that preps it
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
	%Run the node prep thats in the Ruby project if it exists: project/.distributest/node_prep
	%We don't run the global scripts if the project has it.
	ProjectsSetupFile = ProjectFilePath ++ "/" ++ ?PROJECTS_NODE_SETUP_FILE,
  case shell_command:run_file_if_exists(ProjectFilePath, ProjectsSetupFile) of
	  {error, enoent} -> ok; %file doesn't exist so go on
	  _Any -> 
	    notify_done_with_prep_scrip(MasterPrepProcess),
	    exit(normal)
	end,
	%This is rsynced to remote nodes into project:global_setup_script_path() before being ran
	%Only runs if the node prep script wasn't in the project
	shell_command:run_file_if_exists(ProjectFilePath, GlobalSetupScriptPath ++ ?GLOBAL_NODE_SETUP_FILE),
	notify_done_with_prep_scrip(MasterPrepProcess).
	
notify_done_with_prep_scrip(Pid) ->
  Pid ! done_with_prep_script.

node_ready(Host, RunnerCount, MasterPid) ->
	MasterPid ! {node_ready, Host, RunnerCount}.

%% @spec node_name(Host) -> RemoteVmName
%% where Host = string()
%%       RemoteVmName = string()
%%
%% @doc Takes a host name and returns what the remote vm name should be
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