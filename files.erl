-module(files).
-include_lib("kernel/include/file.hrl").
-export([test_files/0]).

test_files() ->
	Files = filelib:wildcard(configuration:test_files_glob()),
	file_sort_on_size(Files).
	
file_sort_on_size(Files) ->
  SizeFun = fun(A,B) -> file_size(A) =< file_size(B) end,
  lists:reverse(lists:sort(SizeFun, Files)).
  
file_size(File) ->
  {ok, R} = file:read_file_info(File),
  #file_info{size=Size} = R,
  Size.