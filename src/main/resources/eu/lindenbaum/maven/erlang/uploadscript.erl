Node = %s,
BeamFileList = %s,
AppFileList = %s,
ResourceList = %s,

%% --------------------------------------------------------------------------
%% Utility Section
%% --------------------------------------------------------------------------

DirPredicate =
fun(Path) when is_list(Path) ->
        rpc:call(Node, filelib, is_dir, [Path]);
   (_) ->
        false
end,

GetEnvDirs = 
fun() ->
        Vars = ["TMPDIR", "TMP", "TEMP"],
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

%% Returns the best possible temporary directory, as resolved by checking
%% the 1) environment settings, 2) commonly used paths or 3) current
%% working directory of the remote node; or the atom 'error' if no
%% temp directory could be found.
GetTmpDir =
fun() ->
        case GetEnvDirs() ++ GetDefaultDirs() ++ GetRemoteCwd() of
            [Tmp | _] ->
                Tmp;
            _ ->
                error
        end
end,

%% Parses a given resource (file) and temp directory into a tuple containing
%% the target root path per dependency and the relative path with the target
%% resource.
%%
%% Example:
%%      $PROJ/target/lib/dep1/priv/resource
%%   yields:
%%      {$TMP/dep1/, priv/resource}
%%
GetTargetResourcePath =
fun(Resource, TmpDir) ->
        File = filename:basename(Resource),
        Path = filename:dirname(Resource),
        Es = lists:reverse(filename:split(Path)),
        {PrivPath, Dependency} = 
            lists:foldl(fun(E, {PP = ["priv" | _], []}) ->
                                {PP, [E]};
                           (E, {PP, []}) ->
                                {[E | PP], []};                           
                           (_, {PP, Dep}) ->
                                {PP, Dep}
                        end, {[], []}, Es),
        TargetPath = rpc:call(Node, filename, join, [[TmpDir, Dependency]]),
        RelativeResource = rpc:call(Node, filename, join, [[PrivPath, File]]),
        {TargetPath, RelativeResource}
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
                {TargetPath, RelativeFile} = GetTargetResourcePath(Resource, TmpDir),
                Target = rpc:call(Node, filename, join, [[TargetPath, RelativeFile]]),
                case rpc:call(Node, filelib, ensure_dir, [Target]) of
                    ok ->
                        WriteArgs = [Target, Binary],
                        R = rpc:call(Node, file, write_file, WriteArgs),
                        case R of
                            ok ->
                                ModeArgs = [Target, 8#00755],
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
        {TargetPath, RelativeFile} = GetTargetResourcePath(Resource, TmpDir),
        case rpc:call(Node, code, add_patha, [TargetPath]) of
            true ->
                ok;
            {error, bad_directory} ->
                {error, bad_directory, TargetPath}
        end
end,

%% --------------------------------------------------------------------------
%% Script Section
%% --------------------------------------------------------------------------

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
                          ok = AddResourceToCodePath(Resource, Tmp),
                          {[Resource | Succeeded], Failed};
                      Error ->
                          {Succeeded, [Error | Failed]}
                  end
          end, B, ResourceList);
    false ->
        {error, {failed_to_connect_to, Node}}
end.
