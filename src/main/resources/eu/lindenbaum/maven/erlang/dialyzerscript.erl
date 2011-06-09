Directories = %s,
Includes = %s,
WarnOptions = %s,
Options = [{from, src_code},
           {get_warnings, true},
           {files_rec, Directories},
           {include_dirs, Includes},
           {warnings, WarnOptions}],
lists:map(
  fun(Warning) ->
	  S = dialyzer:format_warning(Warning),
	  lists:flatten(S)
  end,
  dialyzer:run(Options)).
