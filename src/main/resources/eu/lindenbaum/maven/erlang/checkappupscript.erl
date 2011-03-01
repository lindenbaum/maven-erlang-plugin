AppUpFile = "%s",
Version = "%s",
lists:flatten(
  case file:consult(AppUpFile) of
      {ok, [{Version, Up, Down}]} when is_list(Up) andalso is_list(Down) ->
          lists:foldl(
            fun({V, Is}, []) when is_list(Is) ->
                    lists:foldl(
                      fun(E, []) when is_tuple(E) ->
			      [];
                         (E, []) ->
                              io_lib:format("malformed instruction in ~p: ~p", [V, E]);
                         (_, Acc) ->
			      Acc
		      end,  [], Is);
	       (T, []) ->
		    io_lib:format("malformed entry ~p", [T]);
               (_, Acc) ->
		    Acc
	    end, [], Up ++ Down);
      {ok, [{V, _, _}]} ->
	  io_lib:format(".appup has invalid version ~p", [V]);
      {ok, _} ->
	  ".appup file is malformed";
      {error, E = {_, _, _}} ->
	  file:format_error(E);
      {error, Reason} ->
	  io_lib:format("file:consult/1 failed with ~p", [Reason])
  end).
