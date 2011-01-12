-module(shell_command).
-export([run/2, rsync_remote/1, rsync_local/0]).

run(Dir, Cmd) ->
  run(Dir, Cmd, 120000).

run(Dir, Cmd, Timeout) ->
  Port = erlang:open_port({spawn, Cmd}, [{cd, Dir}, exit_status]),
  loop(Cmd, Port,[],Timeout).
  
loop(Cmd, Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} -> loop(Cmd, Port, Data++NewData, Timeout);
      {Port, {exit_status, 0}} -> Data;
    	{Port, {exit_status, S}} -> throw({commandfailed, Cmd, S})
    after Timeout ->
    	throw(timeout)
    end.

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