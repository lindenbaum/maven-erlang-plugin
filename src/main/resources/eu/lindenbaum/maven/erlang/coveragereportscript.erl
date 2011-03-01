Dir = "%s",
Tests = %s,
Modules = %s,
case cover2:compile_beam_directory(Dir, [debug_info, export_all, {d, 'TEST'}]) of 
   {error, Reason} ->
       {error, [lists:flatten(io_lib:format("~p", [Reason]))]};
   _ ->
       case catch(eunit:test(Tests)) of 
       ok ->
           begin 
           Levels = [module, function, clause, line],
           Results = [{L, cover2:analyse(M, coverage, L)}
                  || M <- Modules, L <- Levels],
           SortedResults = 
               lists:foldl(
                 fun 
                 (Mod = {module, _}, {Ms, Fs, Cs, Ls}) ->
                          {[Mod | Ms], Fs, Cs, Ls};
                 (Fun = {function, _}, {Ms, Fs, Cs, Ls}) ->
                          {Ms, [Fun | Fs], Cs, Ls};
                 (Clause = {clause, _}, {Ms, Fs, Cs, Ls}) ->
                          {Ms, Fs, [Clause | Cs], Ls};
                 (Line = {line, _}, {Ms, Fs, Cs, Ls}) ->
                          {Ms, Fs, Cs, [Line | Ls]};
                 (stop, {Ms, Fs, Cs, Ls}) ->
                          Ls ++ Cs ++ Fs ++ Ms
                      end,
                 {[], [], [], []},
                 Results ++ [stop]),      
           Table =
               lists:foldl(
                 fun 
                 ({L, {ok, {M, {C, N}}}}, Acc) ->
                          [[{L, M, C, N}] | Acc];
                 ({L, {ok, Cov}}, Acc) ->
                          Rows =
                          lists:map(fun({{M, F, A}, {C, N}}) ->
                                    {L, M, F, A, C, N};
                               ({{M, F, A, I}, {C, N}}) ->
                                    {L, M, F, A, I, C, N};
                               ({{M, Ln}, {C, N}}) ->
                                    {L, M, Ln, C, N}
                                end,
                                Cov),
                          [Rows | Acc]
                      end,
                 [],
                 SortedResults),
           {ok, lists:flatten(Table)}
           end;
       error ->
           {error, []};
       {Class, Reason} -> 
           {error, [lists:flatten(io_lib:format("~p:~p", [Class, Reason]))]};
       {error, Reason} ->
           {error, [lists:flatten(io_lib:format("~p", [Reason]))]}
       end 
end.
