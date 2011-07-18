Node = %s,
BeamFileList = %s,
AppFileList = %s,
ResourceList = %s,

%%--------------------------------------------------------------------------
%% Utility Section
%%--------------------------------------------------------------------------

DirPredicate =
fun(Path) when is_list(Path) ->
	rpc:call(Node, filelib, is_dir, [Path]);
   (_) ->
	false
end,

GetEnvDirs = 
fun() ->
	Vars = ["$TMPDIR", "$TMP", "$TEMP"],
	Paths = [rpc:call(Node, os, getenv, [Var]) || Var <- Vars],
	lists:filter(DirPredicate, Paths)
end,

GetDefaultDirs =
fun() ->
	Paths = ["/tmp", "/var/tmp"],
	lists:filter(DirPredicate, Paths)
end,

GetRemoteCwd =
fun() ->
	case rpc:call(Node, file, get_cwd, []) of
	    {ok, Cwd} ->
		[Cwd];
	    _ ->
		[]
	end
end,

GetTmpDir =
fun() ->
	case GetEnvDirs() ++ GetDefaultDirs() ++ GetRemoteCwd() of
	    [Tmp | _] ->
		Tmp;
	    _ ->
		error
	end
end,

GetTargetResourcePath =
fun(Resource, TmpDir) ->
	Regex = "target/(lib/)?(.+/priv/.+)",
	Opts = [{capture, all_but_first}],
	{match, [_ | [{S, L} | _]]} = re:run(Resource, Regex, Opts),
	Relative = string:substr(Resource, S + 1, L),
	rpc:call(Node, filename, join, [[TmpDir, Relative]])
end,

GetTargetResourceRoot =
fun(Resource, TmpDir) ->
	Regex = "target/(.+)/",
	Opts = [{capture, all_but_first}, ungreedy],
	{match, [{S, L} | _]} = re:run(Resource, Regex, Opts),
	Root = string:substr(Resource, S + 1, L),
	rpc:call(Node, filename, join, [[TmpDir, Root]])
end,

UploadModule =
fun(BeamFile) ->
	Module = filename:basename(BeamFile, ".beam"),
	case file:read_file(BeamFile) of
	    {ok, Binary} ->
		ModuleName = list_to_atom(Module),
		LoadArgs = [ModuleName, BeamFile, Binary],
		rpc:call(Node, code, purge, [ModuleName]),
		rpc:call(Node, code, load_binary, LoadArgs),
		BeamFile;

	    Error ->
		{BeamFile, Error}
	end
end,

LoadApp =
fun(AppFile) ->
	case file:consult(AppFile) of
	    {ok, [AppSpec]} ->
		rpc:call(Node, application, load, [AppSpec]),
		AppFile;

	    Error ->
		{AppFile, Error}
	end
end,

UploadResource =
fun(Resource, TmpDir) when is_list(TmpDir) ->
	case file:read_file(Resource) of
	    {ok, Binary} ->
		Target = GetTargetResourcePath(Resource, TmpDir),
		case rpc:call(Node, filelib, ensure_dir, [Target]) of
		    ok ->
			WriteArgs = [Target, Binary],
			R = rpc:call(Node, file, write_file, WriteArgs),
			case R of
			    ok ->
				ModeArgs = [Target, 755],
				rpc:call(Node, file, change_mode, ModeArgs),
				Resource;

			    Error ->
				{Resource, Error}
			end;

		    Error ->
			{Resource, Error}
		end;

	    Error ->
		{Resource, Error}
	end;

   (Resource, _) ->
	{Resource, {skipped, no_tmp_directory_available}}
end,

AddResourceToCodePath =
fun(Resource, TmpDir) ->
	Root = GetTargetResourceRoot(Resource, TmpDir),
	Result = rpc:call(Node, code, add_patha, [Root])
end,

%%--------------------------------------------------------------------------
%% Script Section
%%--------------------------------------------------------------------------

Tmp = GetTmpDir(),
case net_kernel:connect(Node) of
    true ->
	A = lists:foldl(
	      fun(Beam, {Succeeded, Failed}) ->
		      case UploadModule(Beam) of
			  Beam ->
			      {[Beam | Succeeded], Failed};

			  Error ->
			      {Succeeded, [Error | Failed]}
		      end
	      end, {[], []}, BeamFileList),
	B = lists:foldl(
	      fun(AppFile, {Succeeded, Failed}) ->
		      case LoadApp(AppFile) of
			  AppFile ->
			      {[AppFile | Succeeded], Failed};

			  Error ->
			      {Succeeded, [Error | Failed]}
		      end
	      end, A, AppFileList),
	lists:foldl(
	  fun(Resource, {Succeeded, Failed}) ->
		  case UploadResource(Resource, Tmp) of
		      Resource ->
			  AddResourceToCodePath(Resource, Tmp),
			  {[Resource | Succeeded], Failed};

		      Error ->
			  {Succeeded, [Error | Failed]}
		  end
	  end, B, ResourceList);

    false ->
	{error, {failed_to_connect_to, Node}}
end.
