Node = %s,
BeamFileList = %s,
ApplicationFileList = %s,
case net_kernel:connect(Node) of
    true ->
        E = lists:foldl(
              fun(BeamFile, Ok) when is_list(Ok) ->
                      Module = filename:basename(BeamFile, ".beam"),
                      case file:read_file(BeamFile) of
                          {ok, Binary} ->
                              rpc:call(
                                Node, code, purge,
                                [list_to_atom(Module)]),
                              rpc:call(
                                Node, code, load_binary,
                                [list_to_atom(Module),
                                 BeamFile, Binary]),
                              [BeamFile | Ok];
                          Other ->
                              {BeamFile, Other}
                      end;
		 (_, Error) ->
		      Error
	      end, [], BeamFileList),
	lists:foldl(
	  fun(AppFile, Ok) when is_list(Ok) ->
		  case file:consult(AppFile) of
		      {ok, [AppSpec]} ->
			  rpc:call(
			    Node, application,
			    load, [AppSpec]),
			  [AppFile | Ok];
		      Other ->
			  {AppFile, Other}
		  end;
	     (_, Error) ->
		  Error
	  end, E, ApplicationFileList);
    false ->
	{error, {cannot_connect, Node}}
end.
