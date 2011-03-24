Relup = "%s",
Rel = "%s",
Releases = %s,
Paths = %s,
case systools:make_relup(Rel, Releases, Releases, Paths ++ [silent, noexec]) of
    {ok, {_, UpFrom, DownTo}, Module, Warnings} ->
	Content = io_lib:format("{${VERSION},\n ~p,\n ~p}.\n", [UpFrom, DownTo]),
	case file:write_file(Relup, Content) of
	    ok ->
		{ok, lists:flatten(Module:format_warning(Warnings))};
	    Error ->
		Error
	end;
    {error, Module, Error} ->
	{error, lists:flatten(Module:format_error(Error))}
end.
