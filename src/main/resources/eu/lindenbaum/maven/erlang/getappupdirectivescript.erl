Module = '%s',
Path = "%s",
code:purge(Module),
code:delete(Module),
code:purge(Module),
case code:load_abs(filename:join([Path, atom_to_list(Module)])) of
    {module, M} ->
	Attributes = M:module_info(attributes),
	Exports = M:module_info(exports),
	IsSupBE = lists:member({behaviour, [supervisor]}, Attributes),
	IsSupAE = lists:member({behavior, [supervisor]}, Attributes),
	IsUpdateable = lists:member({code_change, 3}, Exports),
	case IsSupAE orelse IsSupBE of
	    true ->
		{update, M, supervisor};
	    false when IsUpdateable ->
		{update, M};
	    false ->
		{load_module, M}
	end;
    {error, Reason} ->
	error
end.
