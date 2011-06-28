Node = %s,
Applications = %s,
SysConfigFile = "%s",

%%------------------------------------------------------------------------------
%% Utility Section
%%------------------------------------------------------------------------------

%% F = fun(Applications, StatusAcc, F) -> ok | Error
Start =
fun([], Acc, _) ->
	Acc;

   ([A | Rest], Acc = {Succeeded, Failed}, Rec) ->
	case rpc:call(Node, application, start, [A]) of
	    ok ->
		Rec(Rest, {[A | Succeeded], Failed}, Rec);

	    {error, {already_started, _}} ->
		Rec(Rest, Acc, Rec);

	    {error, {not_started, Dep}} ->
		Rec([Dep, A] ++ Rest, Acc, Rec);

	    {error, Err = {"no such file or directory", AppFile}} ->
		Msg = "maybe dependencies are missing, try a re-run using '-DwithDependencies'",
		{Succeeded, [{A, {Err, Msg}} | Failed]};

	    Error ->
		{Succeeded, [{A, Error} | Failed]}
	end
end,

Apply =
fun({Application, ParValList}) ->
	lists:foreach(
	  fun({Par, Val}) ->
		  rpc:call(Node, application, set_env, [Application, Par, Val])
	  end, ParValList)
end,


Configure =
fun(SysConfig) ->
	case filelib:is_regular(SysConfig) of
	    true ->
		case file:consult(SysConfig) of
		    {ok, [Config]} ->
			lists:foreach(Apply, Config);

		    Error ->
			{SysConfig, Error}
		end;

	    _ ->
		ok
	end
end,

%%------------------------------------------------------------------------------
%% Script Section
%%------------------------------------------------------------------------------

case Configure(SysConfigFile) of
    ok ->
	{S, F} = Start(Applications, {[], []}, Start),
	{lists:reverse(S), lists:reverse(F)};

    Error ->
	{[], [Error]}
end.
