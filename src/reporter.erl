-module(reporter).
-export([start/0, sort_profile/2]).

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
  
loop(TimePerFile, Profiles) ->
	receive
		{pass_results, Text} ->
      io:format("~s", [Text]),
      loop(TimePerFile, Profiles);

    {fail_results, Text} ->
	    io:format("~n  ~p~n", [Text]),
	    loop(TimePerFile, Profiles);
	  
	  %TODO: log this to file
	  {total_time_for_file, File, Time} ->
		  loop([{File, Time} | TimePerFile], Profiles);
     
    {profile, File, Profile} ->
	    loop(TimePerFile, [sort_profile(File, Profile) | Profiles]);
	
 	  {shutdown, Caller} -> 
       io:format("~nTOTAL_TIME_PER_FILE, Greater than ~p second(s): ~n~p",[configuration:display_file_time_greater_than(), sort_time_per_file(TimePerFile)]),
       io:format("~nPROFILE TIMES, Greater than ~p second(s): ~n~p~n", [configuration:display_profile_time_greater_than(), remove_empty_profiles(Profiles)]),
      Caller ! reporter_down,
      exit('ShuttingDown')
	      
	end.