-module(shell_command).
-export([run/2, run/4, rsync_remote/1, rsync_local/0, kill_port_process/1]).
-vsn("0.0.3").

-define(TIMEOUT, 120000).

run(Dir, Cmd) ->
	run(Dir, Cmd, ?TIMEOUT).

run(Dir, Cmd, Timeout) ->
  Port = start_port(Cmd, Dir),
  loop(Cmd, Port,[],Timeout).

run(Dir, Cmd, {monitor, MonitorRef}, {identifier, ProcessIdentifier}) ->
  Port = start_port(Cmd, Dir),
  loop_with_monitor(Cmd, Port, [], ?TIMEOUT, MonitorRef, ProcessIdentifier).
  
loop(Cmd, Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} -> loop(Cmd, Port, Data++NewData, Timeout);
    {Port, {exit_status, 0}} -> Data;
    {Port, {exit_status, S}} -> throw({commandfailed, Cmd, S})
  after Timeout ->
    throw(timeout)
  end.

%% If run/4 is called this loop is used
%% If the monitored process goes down this will immediately kill the process on the system 
%% using kill -9 and the Identifier.The identifier is masters hostname + runner number so 
%% this is fairly safe.
loop_with_monitor(Cmd, Port, Data, Timeout, MonitorRef, ProcessIdentifier) ->
  receive
    {Port, {data, NewData}} -> loop_with_monitor(Cmd, Port, Data++NewData, Timeout, MonitorRef, ProcessIdentifier);
    {Port, {exit_status, 0}} -> Data;
    {Port, {exit_status, S}} -> throw({commandfailed, Cmd, S});
    {'DOWN', MonitorRef, process, _Pid, _Reason} -> 
      kill_port_process(ProcessIdentifier),
      exit(master_down)
  after Timeout ->
    throw(timeout)
  end.

start_port(Cmd, Dir) ->
  erlang:open_port({spawn, Cmd}, [{cd, Dir}, exit_status]).

kill_port_process(Identifier) ->
  os:cmd("ps aux | grep " ++ Identifier ++ " | grep -v grep | awk '{print $2}' | xargs kill -9 ").

%%TODO move rsync stuff to diff module
rsync_remote(UserHost) ->
	{ok, ProjectDir} = file:get_cwd(),
	{ok, LocalHostname} = inet:gethostname(),
	run(ProjectDir, "rsync -r --exclude 'log' --exclude '.git' --delete " ++ ProjectDir ++ " " ++ UserHost ++ ":" ++ configuration:remote_dir() ++ LocalHostname).
	
%%to remove after removing local rsyncing
rsync_local() ->
	{ok, ProjectDir} = file:get_cwd(),
	{ok, LocalHostname} = inet:gethostname(),
	run(ProjectDir, "rsync -r --exclude 'log' --exclude '.git' --delete " ++ ProjectDir ++ " " ++ configuration:remote_dir() ++ LocalHostname).