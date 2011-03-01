Attribute = %s,
Modules = %s,
lists:flatten(
  lists:foldl(
    fun(Module, Acc) ->
	    A = Module:module_info(attributes),
	    case proplists:get_value(Attribute, A) of
		undefined ->
		    Acc;
		_ ->
		    [Module | Acc]
	    end
    end, [], Modules)).
