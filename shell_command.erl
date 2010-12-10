-module(shell_command).
-export([run/2]).

run(Dir, Cmd) ->
  run(Dir, Cmd, 60000).

run(Dir, Cmd, Timeout) ->
  Port = erlang:open_port({spawn, Cmd}, [{cd, Dir}, exit_status]),
  loop(Port,[],Timeout).
  
loop(Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} -> loop(Port, Data++NewData, Timeout);
      {Port, {exit_status, 0}} -> Data;
    	{Port, {exit_status, S}} -> throw({commandfailed, S})
    after Timeout ->
    	throw(timeout)
    end.