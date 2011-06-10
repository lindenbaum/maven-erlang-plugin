SurefirePath = "%s",
SuiteName = "%s.",
Tests = %s,
Surefire = {report, {surefire, [{dir, SurefirePath}, {package, SuiteName}]}},
Tty = {report, {ttycapture, [{report_to, self()}]}},

Out = try eunit:test(Tests, [Surefire, Tty]) of
	  error ->
	      ["*test execution failed*", ""];
	  {error, Why} ->
	      Msg = io_lib:format("~p", [Why]),
	      [lists:flatten(Msg), "*test execution failed*", ""];
	  _ ->
	      []
      catch
	  Class:Exception ->
	      Msg = io_lib:format("~p:~p", [Class, Exception]),
	      [lists:flatten(Msg), "*test execution failed*", ""]
      end,
receive
    {Level, Numbers, Captured} ->
	{Level, Numbers, Captured ++ Out}
end.
