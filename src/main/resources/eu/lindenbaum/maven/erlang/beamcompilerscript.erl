OutDir = "%s",
Includes = %s,
CustomOptions = %s,
Files = %s,
Options = [return, {outdir, OutDir}] ++ Includes ++ CustomOptions,
lists:foldl(
  fun(ToCompile, {"", Reports}) ->
          {Fail, Messages} = case compile:file(ToCompile, Options) of
				 {error, E, W} ->
				     {ToCompile,
				      lists:map(fun(Elem) -> {error, Elem} end, E)
				      ++ lists:map(fun(Elem) -> {warn, Elem} end, W)};
				 {ok, M, W} ->
				     Mod = atom_to_list(M),
				     code:load_abs(filename:join([OutDir, Mod])),
				     {"",
				      lists:map(fun(Elem) -> {warn, Elem} end, W)}
                             end,
          R = lists:foldr(
                fun({Level, {File, Exceptions}}, Acc) ->
                        lists:foldr(fun({Line, Module, Info}, A) ->
                                            Formatted = Module:format_error(Info),
                                            Flattened = lists:flatten(Formatted),
                                            F = io_lib:format("~s:~p:", [File, Line]),
                                            S = io_lib:format("~s", [Flattened]),
                                            [{Level, lists:flatten(F)},
                                             {Level, lists:flatten(S)}] ++ A;
                                       (Else, A) ->
                                            F = io_lib:format("~s:", [File]),
                                            S = io_lib:format("~p", [Else]),
                                            [{Level, lists:flatten(F)},
                                             {Level, lists:flatten(S)}] ++ A
                                    end, Acc, Exceptions)
                end, [], Messages),
          {Fail, Reports ++ R};
     (_, Result) ->
          Result
  end, {"", []}, Files).
