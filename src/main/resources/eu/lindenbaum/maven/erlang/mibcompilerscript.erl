OutDir = "%s",
Includes = %s,
Files = %s,

Options = [{outdir, OutDir}, {i, Includes}],

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
        case snmpc:compile(Src, Options) of
            {error, Reason} ->
                {error, Src, Reason};
            {ok, _MibBinary} ->
                {ok, Src}
        end
end,

%% formats the report returned by the compiler function
FormatCompileReport =
fun(Msgs) ->
        %% TODO: Parse and format errors.
        [FormatFlatten(" * ~p", [Msgs])]
end,

%%--------------------------------------------------------------------------
%% Script Section
%%--------------------------------------------------------------------------

lists:foldl(
  fun(Source, {Failed, Compiled, Errors}) ->
          case Compile(Source) of
              {ok, Source} ->
                  {Failed, [Source | Compiled], Errors};
              {error, Source, Reason} ->
                  {[Source | Failed], Compiled, Errors ++ FormatCompileReport(Reason)}
          end
  end, {[], [], []}, Files).
