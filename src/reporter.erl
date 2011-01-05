-module(reporter).
-export([start/0]).

start() ->
	 spawn(fun() -> loop([]) end).

loop(TimePerFile) ->
	receive
		{pass_results, Text} ->
      io:format("~s", [Text]),
      loop(TimePerFile);

    {fail_results, Text} ->
	    io:format("~n  ~p~n", [Text]),
	    loop(TimePerFile);
	  
	  %TODO: log this to file
	  {total_time_for_file, File, Time} ->
		  loop([{File, Time} | TimePerFile]);
%		  loop(TimePerFile);
	
 	  {shutdown, Caller} -> 
       io:format("~nTOTAL_TIME_PER_FILE~n~p",[TimePerFile]),
      Caller ! reporter_down,
      exit('ShuttingDown')
	      
	end.