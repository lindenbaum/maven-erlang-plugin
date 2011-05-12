SurefirePath = "%s",
SuiteName = "%s.",
Tests = %s,
Surefire = {report, {surefire, [{dir, SurefirePath}, {package, SuiteName}]}},
Tty = {report, {ttycapture, [{report_to, self()}]}},
Out = try eunit:test(Tests, [Surefire, Tty]) of
          error -> ["Test(s) failed!"];
          _ -> []
      catch
          Class:Exception ->
              Msg = io_lib:format("~p:~p", [Class, Exception]),
              [lists:flatten(Msg)]
      end,
receive
    {Level, Captured} ->
	{Level, Captured ++ Out}
end.
