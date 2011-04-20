%% @author Curtis Carter <curtis@rubyhq.com>. 
%% @doc Handles the output of tests. <br/>

-module(reporter).
-export([start/0]).
-vsn("0.0.5").

%% @doc Immediately spawns a new process and goes into loop
start() ->
	 spawn_link(fun() -> loop([],[], 0, 0) end).
	
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
	case Error of
  	{Message, Results, Exception} ->
    	io:format("~n~p~n~s~n~s~n", 
		           [binary_to_list(Message), 
    			      binary_to_list(Results),
			  	   	  binary_to_list(Exception)]);
		Any -> io:format("~n~s~n", [Any])
	end,
	failed_message(Errors).
	
display_time_per_file(TimePerFile) ->
  FileTimesToDisplay = sort_time_per_file(TimePerFile),
	case FileTimesToDisplay of
	  [] -> ok;
	  _Any -> 
	    io:format("~nTOTAL_TIME_PER_FILE, Greater than ~p second(s): ~n~p",
	              [configuration:display_file_time_greater_than(), FileTimesToDisplay])
  end.

display_profile_times(ProfileTimes) ->
	ProfilesToDisplay = remove_empty_profiles(ProfileTimes),
	case ProfilesToDisplay of
		[] -> ok;
		_Any -> 
		  io:format("~nPROFILE TIMES, Greater than ~p second(s): ~n~p~n",
		            [configuration:display_profile_time_greater_than(), remove_empty_profiles(ProfilesToDisplay)])
	end.
  
	
loop(TimePerFile, Profiles, PassCount, FailCount) ->
	receive
		{pass_results, Text} ->
			NewPassCount = PassCount + length(binary_to_list(Text)),
      io:format("~s", [Text]),
      loop(TimePerFile, Profiles, NewPassCount, FailCount);

    {fail_results, Text} ->
	    FailedList = tuple_to_list(Text),
	    NewFailCount = FailCount + length(FailedList),
  	  failed_message(FailedList),
      loop(TimePerFile, Profiles, PassCount, NewFailCount);
	  
	  {total_time_for_file, File, Time} ->
		  loop([{File, Time} | TimePerFile], Profiles, PassCount, FailCount);
     
    {profile, File, Profile} ->
	    loop(TimePerFile, [sort_profile(File, Profile) | Profiles], PassCount, FailCount);
	
	  {captured_std_err_out, Text, RunnerPid} ->
		  error_logger:info_msg("Output from ruby on ~p:~p~n~s", [node(RunnerPid), RunnerPid, Text]),
		  loop(TimePerFile, Profiles, PassCount, FailCount);
		
		{file_put_back_in_queue, File} ->
			error_logger:info_msg("Because a runner died it's file was put back in queue. File: ~p~n", [File]),
			loop(TimePerFile, Profiles, PassCount, FailCount);
			
		{load_error, LoadError} ->
			error_logger:error_msg("There was a GEM load error. ~n~p", [LoadError]),
			io:format("There was a GEM load error. ~n~p~n", [LoadError]),
			loop(TimePerFile, Profiles, PassCount, FailCount);
	
 	  {shutdown, Caller} ->
	    files:log_file_time(TimePerFile),
	    display_time_per_file(TimePerFile),
	    display_profile_times(Profiles),
      io:format("~n~p Passing Tests~n", [PassCount]),
      io:format("~p Failing Tests~n", [FailCount]),
      Caller ! reporter_down,
      exit('ShuttingDown')
	      
	end.