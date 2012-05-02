OutDir = "%s",
Includes = %s,
CustomOptions = %s,
Files = %s,
FirstFiles = %s,

Options = [return, {outdir, OutDir}] ++ Includes ++ CustomOptions,

%%--------------------------------------------------------------------------
%% Utility Section
%%--------------------------------------------------------------------------

%% returns the flattened output of io_lib:format
FormatFlatten =
fun(Format, Args) ->
	lists:flatten(io_lib:format(Format, Args))
end,

%% compiles a source file
Compile =
fun(Src) ->
	case compile:file(Src, Options) of
	    {error, E, W} ->
		{Src, E, W};
	    {ok, M, W} ->
		{Src, [], W}
	end
end,

%% formats the report returned by the compiler function
FormatCompileReport =
fun(Msgs) ->
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
TargetFilePath =
fun(File, Extension) ->
	ModuleName = filename:basename(File, Extension),
	filename:join(OutDir, ModuleName)
end,

%% retrieve a list of attributes from the abstract forms of a specific
%% source file
GetModuleAttributes =
fun(SrcFile) ->
        case epp_dodger:quick_parse_file(SrcFile) of
            {ok, Forms} ->
                lists:foldl(
                  fun({attribute, _, export, Exports}, Acc) ->
                          [{export, E} || E <- Exports] ++ Acc;
                     ({attribute, _, callback, _}, Acc) ->
                          [{export, {behaviour_info, 1}}] ++ Acc;
                     (_, Acc) ->
                          Acc
                  end, [], Forms);
            _ ->
                []
        end
end,

%% returns true if a source module specifies one of the given attributes,
%% attributes must be in the form {export, {Fun, Arity}},
%% {behaviour, BehaviourName}, ...
Defines =
fun(File, Attributes) ->
	A = GetModuleAttributes(File),
	lists:any(
	  fun(Attr) -> lists:member(Attr, A) end,
	  Attributes)
end,

%% sorts the files to compile so that dependencies between modules are
%% resolved during compilation
SortFiles =
fun(Files) ->
	ParseTransform = [{export, {parse_transform, 2}}],
	{Pts, NonPts} = lists:partition(
			  fun(File) ->
				  Defines(File, ParseTransform)
			  end, Files),
	Behaviour = [{export, {behaviour_info, 1}}],
	{Bhvs, NonBhvs} = lists:partition(
			    fun(File) ->
				    Defines(File, Behaviour)
			    end, NonPts),
	Pts ++ Bhvs ++ NonBhvs
end,

%% loads the compiled file, for the given source, from the current target
%% dir, ensuring that the code is properly purged and deleted before.
Load =
fun(Source) ->
        Module = filename:basename(Source, ".erl"),
        Target = TargetFilePath(Source, ".erl"),
        case code:load_abs(Target) of
            {module, _} ->
                ok;
            {error, not_purged} ->
                code:purge(Module),
                code:delete(Module),
                code:purge(Module),
                {module, _} = code:load_abs(Target),
                ok;
            {error, Reason} ->
                {error, Reason}
        end
end,

%%--------------------------------------------------------------------------
%% Script Section
%%--------------------------------------------------------------------------

lists:foldl(
  fun(Source, {Failed, Compiled, Errors, Warnings}) ->
          case Compile(Source) of
              {Source, [], []} ->
                  ok = Load(Source),
                  {Failed,
                   [Source | Compiled],
                   Errors,
                   Warnings};
              {Source, [], W} ->
                  ok = Load(Source),
                  {Failed, 
                   [Source | Compiled],
                   Errors, Warnings ++ FormatCompileReport(W)};
              {Source, E, W} ->
                  {[Source | Failed],
                   Compiled,
                   Errors ++ FormatCompileReport(E),
                   Warnings ++ FormatCompileReport(W)}
          end
  end, {[], [], [], []}, FirstFiles ++ SortFiles(Files)).
