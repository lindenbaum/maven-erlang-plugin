OutDir = "%s",
Includes = %s,
CustomOptions = %s,
Files = %s,

Options = [return, {outdir, OutDir}] ++ Includes ++ CustomOptions,

%% returns the flattened ouutput of io_lib:format
FormatFlatten = fun(Format, Args) ->
			lists:flatten(io_lib:format(Format, Args))
		end,

%% compiles a source file
Compile = fun(Src) ->
		  case compile:file(Src, Options) of
		      {error, E, W} ->
			  {Src, E, W};
		      {ok, M, W} ->
			  {Src, [], W}
		  end
	  end,

%% formats the report returned by the compiler function
FormatCompileReport = fun(Msgs) ->
			      lists:foldl(
				fun({File, Exceptions}, Acc) ->
					lists:foldr(
					  fun({Line, M, Info}, A) ->
						  Flat = lists:flatten(M:format_error(Info)),
						  [FormatFlatten(" * ~s:~p", [File, Line]),
						   FormatFlatten("   ~s", [Flat])] ++ A;
					     (Else, A) ->
						  [FormatFlatten(" * ~s", [File]),
						   FormatFlatten("   ~p", [Else])] ++ A
					  end, Acc, Exceptions)
				end, [], Msgs)
		      end,

%% returns the expected target file path for a file with a given
%% extension excluding the beam suffix
TargetFilePath = fun(File, Extension) ->
			 ModuleName = filename:basename(File, Extension),
			 filename:join(OutDir, ModuleName)
		 end,

%% returns true if the given source file exports the specific
%% function, defined by {Fun, Arity}.
ModuleExports = fun(SrcFile, {Fun, Arity} = Function) ->
			case epp_dodger:quick_parse_file(SrcFile) of
			    {ok, Forms} ->
				E = lists:foldl(
				      fun({attribute, _, export, Es}, Acc) ->
					      Es ++ Acc;
					 (_, Acc) ->
					      Acc
				      end, [], Forms),
				lists:member(Function, E);
			    _ ->
				false
			end
		end,

%% sorting function that places source files defining custom
%% behaviours to the front of the list
BehaviourModulesFirst = fun(SrcFiles) ->
				lists:foldl(
				  fun(SrcFile, Acc) ->
					  case ModuleExports(SrcFile, {behaviour_info, 1}) of
					      true ->
						  [SrcFile | Acc];
					      false ->
						  Acc ++ [SrcFile]
					  end
				  end, [], SrcFiles)
			end,

%% sorting function that places source files exporting parse_transform/2
%% at the front of the list
ParseTransformModulesFirst = fun(SrcFiles) ->
				     lists:foldl(
				       fun(SrcFile, Acc) ->
					       case ModuleExports(SrcFile, {parse_transform, 2}) of
						   true ->
						       [SrcFile | Acc];
						   false ->
						       Acc ++ [SrcFile]
					       end
				       end, [], SrcFiles)
			     end,

%% sorts the files to compile so that dependencies between modules are
%% resolved during compilation
SortFiles = fun(SrcFiles) ->
		    ParseTransformModulesFirst(BehaviourModulesFirst(SrcFiles))
	    end,

%% iterates over the files and compiles
lists:foldl(
  fun(Source, {Failed, Compiled, Errors, Warnings}) ->
	  case Compile(Source) of
	      {Source, [], []} ->
		  code:load_abs(TargetFilePath(Source, ".erl")),
		  {Failed,
		   [Source|Compiled],
		   Errors,
		   Warnings};
	      {Source, [], W} ->
		  code:load_abs(TargetFilePath(Source, ".erl")),
		  {Failed,
		   [Source|Compiled],
		   Errors,
		   Warnings ++ FormatCompileReport(W)};
	      {Source, E, W} ->
		  {[Source|Failed],
		   Compiled,
		   Errors ++ FormatCompileReport(E),
		   Warnings ++ FormatCompileReport(W)}
	  end
  end, {[], [], [], []}, SortFiles(Files)).
