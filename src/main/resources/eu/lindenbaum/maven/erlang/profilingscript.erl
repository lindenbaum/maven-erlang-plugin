ProfilingPath = "%s",
ProfilingFile = "PROFILING-%s.txt",
Tests = %s,
Timeout = %s * 1000,
Tty = {report, {ttycapture, [{report_to, self()}]}},
Root = spawn(fun() ->
		     receive
			 {start, From} ->
			     try eunit:test(Tests, [Tty]) of
				 _ -> From ! {info, ["Tests run sucessfully!"]}
			     catch
				 Class:Exception ->
				     From ! {error, [lists:flatten(io_lib:format("~p:~p", [Class, Exception]))]}
			     end
		     end
	     end),
eprof:start(),
eprof:start_profiling([Root]),
Root ! {start, self()},
Result = receive
	     Res ->
		 Res
	 after Timeout ->
	 	 {error, ["Timeout, waiting for results."]}
	 end,
eprof:stop_profiling(),
eprof:log(filename:join([ProfilingPath, ProfilingFile])),
eprof:analyze(total),
eprof:stop(),
Result.
