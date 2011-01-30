-module(reporter).
-export([start/0]).
-vsn("0.0.5").

start() ->
	 spawn(fun() -> loop([],[]) end).
	
sort_time_fun() ->
  fun({_Description1,Time1}, {_Description2, Time2}) ->
	  Time1 =< Time2 end.

time_greater_than(List, Seconds) ->
  [{Des, Time} || {Des, Time} <- List, Time > Seconds].

sort_time_per_file(FileTime) ->
	Highest = time_greater_than(FileTime, configuration:display_file_time_greater_than()),
  lists:reverse(lists:sort(sort_time_fun(), Highest)).

sort_profile(File, Profile) ->
	Profile2 = tuple_to_list(Profile),
	Highest = time_greater_than(Profile2, configuration:display_profile_time_greater_than()),
  Sorted = lists:reverse(lists:sort(sort_time_fun(), Highest)),
  {File, Sorted}.

%TODO move into above method 
remove_empty_profiles(Profiles) ->
	[{File, Profile} || {File, Profile} <- Profiles, Profile /= []].

failed_message([]) -> ok;
failed_message([Error|Errors]) ->
	{Message, Results, Exception} = Error,
	io:format("~n~p~n~s~n~s~n", 
		        [binary_to_list(Message), 
			       binary_to_list(Results),
			  		 binary_to_list(Exception)]),
	failed_message(Errors).
	
loop(TimePerFile, Profiles) ->
	receive
		{pass_results, Text} ->
      io:format("~s", [Text]),
      loop(TimePerFile, Profiles);

    {fail_results, Text} ->
  	  failed_message(tuple_to_list(Text)),
      loop(TimePerFile, Profiles);
	  
	  {total_time_for_file, File, Time} ->
		  loop([{File, Time} | TimePerFile], Profiles);
     
    {profile, File, Profile} ->
	    loop(TimePerFile, [sort_profile(File, Profile) | Profiles]);
	
	  {captured_std_err_out, Text, RunnerPid} ->
		  error_logger:info_msg("Output from ruby on ~p:~p~n~s", [node(RunnerPid), RunnerPid, Text]),
		  loop(TimePerFile, Profiles);
	
 	  {shutdown, Caller} ->
	    files:log_file_time(TimePerFile),
      io:format("~nTOTAL_TIME_PER_FILE, Greater than ~p second(s): ~n~p",[configuration:display_file_time_greater_than(), sort_time_per_file(TimePerFile)]),
      io:format("~nPROFILE TIMES, Greater than ~p second(s): ~n~p~n", [configuration:display_profile_time_greater_than(), remove_empty_profiles(Profiles)]),
      Caller ! reporter_down,
      exit('ShuttingDown')
	      
	end.