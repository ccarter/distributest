%% @author Curtis Carter <curtis@rubyhq.com>. 
%% @doc Used to run shell commands

-module(shell_command).
-export([run/2, run/4, rsync_remote/1, rsync_local/0, kill_port_process/1, run_file_if_exists/2, run_file_if_exists_with_monitor/5]).
-vsn("0.0.3").

-define(TIMEOUT, 180000).

%% @doc takes directory to run in and command to run and runs it with the default timeout
run(Dir, Cmd) ->
	run(Dir, Cmd, ?TIMEOUT).

run(Dir, Cmd, Timeout) ->
  Port = start_port(Cmd, Dir),
  loop(Cmd, Port,[],Timeout).

%% @spec run(Dir, Cmd, {monitor, MonitorRef}, {identifier, ProcessIdentifier}) -> Data
%% where Dir = string()
%%       Cmd = string()
%%       MonitorRef = reference()
%%       ProcessIdentifier = string()
%%       Data = string()
%%
%% @doc Runs a command and also monitors the process it was called from <br/>
%% If the monitored process goes down the port process will be killed and the script it was running forcefull killed
%% using the ProcessIdentifier to find it on the system BE CAREFULL =).
run(Dir, Cmd, {monitor, MonitorRef}, {identifier, ProcessIdentifier}) ->
  Port = start_port(Cmd, Dir),
  loop_with_monitor(Cmd, Port, [], ?TIMEOUT, MonitorRef, ProcessIdentifier).
  
loop(Cmd, Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} -> loop(Cmd, Port, Data++NewData, Timeout);
    {Port, {exit_status, 0}} -> 
      log_results(0, Cmd, Data),    
      Data;
    {Port, {exit_status, S}} ->
	    log_results(S, Cmd, Data),
	    exit(shell_command_failed)
  after Timeout ->
      exit(shell_command_timeout)
  end.

%% If run/4 is called this loop is used
%% If the monitored process goes down this will immediately kill the process on the system 
%% using kill -9 and the Identifier.The identifier is masters hostname + runner number so 
%% this is fairly safe.
loop_with_monitor(Cmd, Port, Data, Timeout, MonitorRef, ProcessIdentifier) ->
  receive
    {Port, {data, NewData}} -> loop_with_monitor(Cmd, Port, Data++NewData, Timeout, MonitorRef, ProcessIdentifier);
    {Port, {exit_status, 0}} -> 
      log_results(0, Cmd, Data),
      Data;
    {Port, {exit_status, S}} -> 
      log_results(S, Cmd, Data),
 	    exit(shell_command_failed);
    %If Process calling this goes down kill the port forcefully
    {'DOWN', MonitorRef, process, _Pid, _Reason} -> 
      kill_port_process(ProcessIdentifier),
      exit(master_down)
  after Timeout ->
    exit(shell_command_timeout)
  end.

start_port(Cmd, Dir) ->
  erlang:open_port({spawn, Cmd}, [{cd, Dir}, exit_status]).

%% @doc uses os:cmd to forcefull kill a process on the local system using the Identifier to find it
kill_port_process(Identifier) ->
  os:cmd("ps aux | grep " ++ Identifier ++ " | grep -v grep | awk '{print $2}' | xargs kill -9 ").

%% @doc if File exists it is run
run_file_if_exists(ProjectFilePath, File) ->
	FileInfo = file:read_file_info(File),
	case FileInfo of
		{error, enoent} -> ok; %File doesn't exist in project so not going to run it
		{ok, _Fileinfo} -> 
  		shell_command:run(ProjectFilePath, "bash " ++ File ++ " 2>&1")
	end.

%% @doc Run File if exists and monitors caller. If caller goes down port is closed and script forcefully killed using the Identifier
run_file_if_exists_with_monitor(ProjectFilePath, File, RunnerIdentifier, MonitorRef, Identifier) ->
	FileInfo = file:read_file_info(File),
	case FileInfo of
		{error, enoent} -> ok; %File doesn't exist in project so not going to run it
		{ok, _Fileinfo} ->
		  shell_command:run(ProjectFilePath, "bash " ++ File ++ " " ++ RunnerIdentifier ++ " 2>&1", MonitorRef, Identifier)
	end.
	
log_results(0, Cmd, Data) ->
	error_logger:info_msg("Shell Command Completed Successfully~n"
	                      "Shell Command: ~s~n"
                        "Results: ~n~s~n",
                        [Cmd, Data]);
log_results(ExitCode, Cmd, Data) ->
	error_logger:error_msg("Shell Command Failed~n"
	                       "Exit Code: ~p~n"
	                       "Shell Command: ~s~n"
	                       "Results: ~n~s~n",
                         [ExitCode, Cmd, Data]).

%% @doc Remotely rsyncs the Ruby project to the directory the tests will be run from. This includes the local hostname<br/>
%% Also rsyncs the global_setup_scripts to the remote box
rsync_remote(UserHost) ->
	{ok, ProjectDir} = file:get_cwd(),
	{ok, LocalHostname} = inet:gethostname(),
	run(ProjectDir, "rsync -r --exclude 'log' --exclude '.git' --delete " ++ project:local_global_setup_scrip_path() ++ " " ++ UserHost ++ ":" ++ "/tmp/" ++ LocalHostname),
	run(ProjectDir, "rsync -r --exclude 'log' --exclude '.git' --delete " ++ ProjectDir ++ " " ++ UserHost ++ ":" ++ configuration:remote_dir() ++ LocalHostname).
	
%% @doc Locally rsyncs the Ruby project to the directory the tests will be run from. This includes the local hostname<br/>
%% Also rsyncs the global_setup_scripts although this isn't really needed locally
%% @todo Rethink if we want to rsync locally at all
rsync_local() ->
	{ok, ProjectDir} = file:get_cwd(),
	{ok, LocalHostname} = inet:gethostname(),
	run(ProjectDir, "rsync -r --exclude 'log' --exclude '.git' --delete " ++ project:local_global_setup_scrip_path() ++ " " ++ "/tmp/" ++ LocalHostname),
	run(ProjectDir, "rsync -r --exclude 'log' --exclude '.git' --delete " ++ ProjectDir ++ " " ++ configuration:remote_dir() ++ LocalHostname).