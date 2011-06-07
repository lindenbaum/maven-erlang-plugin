SurefirePath = "%s",
SuiteName = "%s.",
Tests = %s,
Surefire = {report, {surefire, [{dir, SurefirePath}, {package, SuiteName}]}},
Tty = {report, {ttycapture, [{report_to, self()}]}},

Sum = fun({P1, F1, S1, C1}, {P2, F2, S2, C2}) ->
	      {P1 + P2, F1 + F2, S1 + S2, C1 + C2}
      end,

lists:foldl(
  fun(Test, {LevelAcc, NumbersAcc, CapturedAcc}) ->
	  Out = try eunit:test(Test, [Surefire, Tty]) of
		    error ->
			Msg = io_lib:format("~p.erl:", [Test]),
			[lists:flatten(Msg), "*failed*", ""];
		    _ ->
			[]
		catch
		    Class:Exception ->
			Msg1 = io_lib:format("~p.erl:", [Test]),
			Msg2 = io_lib:format("~p:~p", [Class, Exception]),
			[lists:flatten(Msg1),
			 "*failed*",
			 lists:flatten(Msg2),
			 ""]
		end,
	  receive
	      {Level, Numbers, Captured} ->
		  {case Level of
		       error -> error;
		       _ when LevelAcc =:= error -> error;
		       warn -> warn;
		       info -> LevelAcc
		   end,
		   Sum(NumbersAcc, Numbers),
		   CapturedAcc ++ Captured ++ Out}
	  end
  end,
  {info, {0, 0, 0, 0}, []}, Tests).
