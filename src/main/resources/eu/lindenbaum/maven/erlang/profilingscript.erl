ProfilingPath = "%s",
ProfilingFile = "PROFILING-%s.txt",
Tests = %s,
Timeout = %s * 1000,

%% spawns a process that roots the eunit test invocations to its pid.
%% all processes and funs that are run as children from this one will
%% be profiled.
Runner = spawn(fun() ->
		       Tty = {report, {ttycapture, [{report_to, self()}]}},
		       receive
			   {start, Starter} ->
			       eunit:stop(), %% note: eunit tests only profiled if spawned AFTER eprof is started.
			       try eunit:test(Tests, [Tty]) of
				   error -> Starter ! {error, nothing, ["Test(s) failed!"]};
				   _ -> ok
			       catch
				   Cl:Ex ->
				       Starter ! {error, nothing, [lists:flatten(io:format("~p:~p", [Cl, Ex]))]}
			       end,
			       receive
				   TestResults -> Starter ! TestResults
			       after Timeout ->
				       Starter ! {error, nothing, ["Timeout, waiting for test results."]}
			       end
		       end
	       end),
{ok, _} = eprof:start(),
profiling = eprof:start_profiling([Runner]),
Runner ! {start, self()},
Result = receive
	     {Level, _, Messages} -> {Level, Messages}
	 after Timeout ->
	 	 {error, ["Timeout, waiting for profiling results."]}
	 end,
eprof:stop_profiling(),
eprof:log(filename:join([ProfilingPath, ProfilingFile])),
eprof:analyze(total),
eprof:stop(),
Result.
