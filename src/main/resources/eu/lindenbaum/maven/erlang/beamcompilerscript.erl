OutDir = "%s",
Includes = %s,
CustomOptions = %s,
Files = %s,

Options = [return, {outdir, OutDir}] ++ Includes ++ CustomOptions,

%% returns the flattened ouutput of io_lib:format
FunA = fun(Format, Args) ->
	       lists:flatten(io_lib:format(Format, Args))
       end,

%% compiles a source file
FunB = fun(Src) ->
	       case compile:file(Src, Options) of
		   {error, E, W} ->
		       {Src, E, W};
		   {ok, M, W} ->
		       {Src, [], W}
	       end
       end,

%% formats the report returned by the compiler function
FunC = fun(Msgs) ->
	       lists:foldl(
		 fun({File, Exceptions}, Acc) ->
			 lists:foldr(
			   fun({Line, M, Info}, A) ->
				   Flat = lists:flatten(M:format_error(Info)),
				   [FunA(" * ~s:~p", [File, Line]),
				    FunA("   ~s", [Flat])] ++ A;
			      (Else, A) ->
				   [FunA(" * ~s", [File]),
				    FunA("   ~p", [Else])] ++ A
			   end, Acc, Exceptions)
		 end, [], Msgs)
       end,

%% returns the expected target file path for a file with a given
%% extension excluding the beam suffix
FunD = fun(File, Extension) ->
	       ModuleName = filename:basename(File, Extension),
	       filename:join(OutDir, ModuleName)
       end,


%% returns true if the given source file defines a custom behaviour
FunE = fun(SrcFile) ->
	       case epp_dodger:quick_parse_file(SrcFile) of
		   {ok, Forms} ->
		       E = lists:foldl(
			     fun({attribute, _, export, Es}, Acc) ->
				     Es ++ Acc;
				(_, Acc) ->
				     Acc
			     end, [], Forms),
		       lists:member({behaviour_info, 1}, E);
		   _ ->
		       false
	       end
       end,

%% sorting function that places source files defining custom
%% behaviours to the front of the list
FunF = fun(SrcFiles) ->
	       lists:foldl(
		 fun(SrcFile, Acc) ->
			 case FunE(SrcFile) of
			     true ->
				 [SrcFile|Acc];
			     false ->
				 Acc ++ [SrcFile]
			 end
		 end, [], SrcFiles)
       end,

%% iterates over the files and compiles
lists:foldl(
  fun(Source, {Failed, Compiled, Errors, Warnings}) ->
	  case FunB(Source) of
	      {Source, [], []} ->
		  code:load_abs(FunD(Source, ".erl")),
		  {Failed,
		   [Source|Compiled],
		   Errors,
		   Warnings};
	      {Source, [], W} ->
		  code:load_abs(FunD(Source, ".erl")),
		  {Failed,
		   [Source|Compiled],
		   Errors,
		   Warnings ++ FunC(W)};
	      {Source, E, W} ->
		  {[Source|Failed],
		   Compiled,
		   Errors ++ FunC(E),
		   Warnings ++ FunC(W)}
	  end
  end, {[], [], [], []}, FunF(Files)).
