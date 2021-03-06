OutDir = "%s",
Files = %s,

%%--------------------------------------------------------------------------
%% Utility Section
%%--------------------------------------------------------------------------

%% returns the flattened output of io_lib:format
FormatFlatten =
fun(Format, Args) ->
        lists:flatten(io_lib:format(Format, Args))
end,

%% compiles an .yrl source file
Compile =
fun(File) ->
        Dest = filename:join([OutDir, filename:rootname(filename:basename(File))]),
        case yecc:file(File, [{parserfile, Dest}, {verbose, true}]) of
            {ok, ParserFile} ->
                {ok, ParserFile};

            {ok, ParserFile, _Warnings} ->
                {ok, ParserFile};

            error ->
                {error, File, "Could not generate."};

            {error, Warnings, Errors} ->
                {error, File, [Errors | Warnings]}
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
              {ok, Result} ->
                  {Failed, [Result | Compiled], Errors};
              {error, Source, Reason} ->
                  {[Source | Failed], Compiled, Errors ++ FormatCompileReport(Reason)}
          end
  end, {[], [], []}, Files).
