-module(monitor).
-export([monitor_runner_node_prep/1]).

monitor_runner_node_prep(Pid) ->
	start_monitor(Pid).
	
start_monitor(Pid) ->
	start_back_up.