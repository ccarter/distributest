-module(reporter).
-compile(export_all).

start() ->
	% process_flag(trap_exit, true),
	 spawn(fun() -> loop() end).

loop() ->
	receive
		{pass_results, Text} ->
      io:format("~s", [Text]),
      loop();

	 % after 0 ->
		%  receive			
		    {fail_results, Text} ->
			    io:format("~n FAILING SPECS ~p~n", [Text]),
			    loop();
			
    	  {shutdown, Caller} -> Caller ! {reporter_down, self()},
        exit('ShuttingDown')
	      
      % after 0 ->
	    %   loop()
     	% end
	end.