Release = "%s",
OutDir = "%s",
Options = [%s],
case systools:make_script(Release, [silent, {outdir, OutDir}] ++ Options) of
    ok ->
	{ok, ""};
    error ->
	{error, "unknown"};
    {ok, Module, Warnings} ->
        {warn, lists:flatten(Module:format_warning(Warnings))};
    {error, Module, Error} ->
        {error, lists:flatten(Module:format_error(Error))}
end.
