OutDir = "%s",
Includes = %s,
CustomOptions = %s,
Files = %s,

Options = [return, {outdir, OutDir}] ++ Includes ++ CustomOptions,

%% returns the flattened ouutput of io_lib:format
F = fun(Format, Args) ->
	    lists:flatten(io_lib:format(Format, Args))
    end,

%% forms the given list into a proplist using the given atom
P = fun(Atom, List) ->
	    lists:map(fun(Elem) -> {Atom, Elem} end, List)
    end,

%% compiles a source file
C = fun(Src) ->
	    case compile:file(Src, Options) of
		{error, E, W} ->
		    {Src, E, W};
		{ok, M, W} ->
		    {Src, [], W}
	    end
    end,

%% formats the report returned by the compiler function
R = fun(Msgs) ->
	    lists:foldl(
	      fun({File, Exceptions}, Acc) ->
		      lists:foldr(
			fun({Line, M, Info}, A) ->
				Flat = lists:flatten(M:format_error(Info)),
				[F(" * ~s:~p", [File, Line]),
				 F("   ~s", [Flat])] ++ A;
			   (Else, A) ->
				[F(" * ~s", [File]),
				 F("   ~p", [Else])] ++ A
			end, Acc, Exceptions)
	      end, [], Msgs)
    end,

%% returns the expected target file path for a source file excluding
%% the beam suffix
B = fun(SrcFile) ->
	    ModuleName = filename:basename(SrcFile, ".erl"),
	    filename:join(OutDir, ModuleName)
    end,


%% returns the expected target file path for a source file
T = fun(SrcFile) ->
            B(SrcFile) ++ ".beam"
    end,

%% returns true if the given source file defines a custom behaviour
Dodger = fun(SrcFile) ->
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
Sort = fun(SrcFiles) ->
	       lists:foldl(
		 fun(SrcFile, Acc) ->
			 case Dodger(SrcFile) of
			     true ->
				 [SrcFile|Acc];
			     false ->
				 Acc ++ [SrcFile]
			 end
		 end, [], SrcFiles)
       end,

%% iterates over the files and compiles
lists:foldl(
  fun(Source, {Failed, Skipped, Compiled, Errors, Warnings}) ->
	  SourceModified = filelib:last_modified(Source),
	  TargetModified = filelib:last_modified(T(Source)),
	  case SourceModified > TargetModified of

	      true ->
		  case C(Source) of
		      {Source, [], []} ->
			  code:load_abs(B(Source)),
			  {Failed,
			   Skipped,
			   [Source|Compiled],
			   Errors,
			   Warnings};
		      {Source, [], W} ->
			  code:load_abs(B(Source)),
			  {Failed,
			   Skipped,
			   [Source|Compiled],
			   Errors,
			   Warnings ++ R(W)};
		      {Source, E, W} ->
			  {[Source|Failed],
			   Skipped,
			   Compiled,
			   Errors ++ R(E),
			   Warnings ++ R(W)}
		  end;

	      _ ->
		  code:load_abs(B(Source)),
		  {Failed,
		   [Source|Skipped],
		   Compiled,
		   Errors,
		   Warnings}
	  end
  end, {[], [], [], [], []}, Sort(Files)).

