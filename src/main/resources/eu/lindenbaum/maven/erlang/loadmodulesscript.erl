Modules = %s,
lists:foldl(
  fun(Module, Acc) ->
	  code:purge(Module),
	  code:delete(Module),
	  code:purge(Module),
	  case code:load_file(Module) of
	      {module, _} -> Acc + 1;
	      _ -> Acc
	  end
  end, 0, Modules).
