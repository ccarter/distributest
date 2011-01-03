-module(reporter).
-compile(export_all).

start() ->
	 spawn(fun() -> loop() end).

loop() ->
	receive
		{pass_results, Text} ->
      io:format("~s", [Text]),
      loop();

    {fail_results, Text} ->
	    io:format("~n  ~p~n", [Text]),
	    loop();
	
 	  {shutdown, Caller} -> Caller ! {reporter_down, self()},
      exit('ShuttingDown')
	      
	end.