Dir = "%s",
Tests = %s,
Modules = %s,
CoveragePath = "%s",
CoverageFile = "COVERAGE-%s.txt",

%% sorts coverage report results into a tuple of four report lists
%% in the following order: {modules, functions, clauses, lines}.
SortCoverageReports = fun(Results) ->
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
				Results ++ [stop])
		      end,

%% flattens sorted results to a non-normalized table with rows
%% that vary in width and content depending on the type (module,
%% function, clause or line).
TableifyCoverageReport = fun(SortedResults) ->
				 lists:foldl(
				   fun 
				       ({Label, {ok, {Mod, {Cov, NotCov}}}}, Acc) ->
					   [[{Label, Mod, Cov, NotCov}] | Acc];
				       ({Label, {ok, Coverage}}, Acc) ->
					   Rows =
					       lists:map(
						 fun						     
						     ({{Mod, Fun, Arity}, {Cov, NotCov}}) ->
							 {Label, Mod, Fun, Arity, Cov, NotCov};
						     ({{Mod, Fun, Arity, Index}, {Cov, NotCov}}) ->
							 {Label, Mod, Fun, Arity, Index, Cov, NotCov};
						     ({{Mod, Ln}, {Cov, NotCov}}) ->
							 {Label, Mod, Ln, Cov, NotCov}
						 end,
						 Coverage),
					   [Rows | Acc]
				   end,
				   [],
				   SortedResults)
			 end,

%% writes the table tuples out to a space separated CSV file where
%% each line is prefixed with it's type (module, function, clause, line)
WriteToFile = fun(TableResults) ->
		      {ok, File} = file:open(filename:join([CoveragePath, CoverageFile]), [write]),
		      lists:map(fun
				    ({module, Mod, Cov, NotCov}) ->
					io:format(File, "module ~p ~p ~p~n", [Mod, Cov, NotCov]);
				    ({function, Mod, Fun, Arity, Cov, NotCov}) ->
					io:format(File, "function ~p ~p ~p ~p ~p~n", [Mod, Fun, Arity, Cov, NotCov]);
				    ({clause, Mod, Fun, Arity, Index, Cov, NotCov}) ->
					io:format(File, "clause ~p ~p ~p ~p ~p ~p~n", [Mod, Fun, Arity, Index, Cov, NotCov]);
				    ({line, Mod, LineNumber, Cov, NotCov}) ->
					io:format(File, "line ~p ~p ~p ~p~n", [Mod, LineNumber, Cov, NotCov])
				end,
				TableResults),
		      ok = file:close(File)
	      end,

case cover2:compile_beam_directory(Dir, [debug_info, export_all, {d, 'TEST'}]) of 
    {error, Reason} ->
	{error, [lists:flatten(io_lib:format("~p", [Reason]))]};
    _ ->
	case catch(eunit:test(Tests)) of 
	    ok ->
		begin 
		    Levels = [module, function, clause, line],
		    Results = [{L, cover2:analyse(M, coverage, L)} || M <- Modules, L <- Levels],
		    SortedResults = SortCoverageReports(Results),
		    Table = TableifyCoverageReport(SortedResults),
		    FlatTable = lists:flatten(Table),
		    ok = WriteToFile(FlatTable),
		    {ok, FlatTable}
		end;
	    error ->
		{error, []};
	    {Class, Reason} -> 
		{error, [lists:flatten(io_lib:format("~p:~p", [Class, Reason]))]};
	    {error, Reason} ->
		{error, [lists:flatten(io_lib:format("~p", [Reason]))]}
	end 
end.
