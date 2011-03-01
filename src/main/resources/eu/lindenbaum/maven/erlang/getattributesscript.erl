Attributes = %s,
Modules = %s,
lists:flatten(
  lists:foldl(
    fun(Module, Acc) ->
	    A = Module:module_info(attributes),
	    lists:foldl(
	      fun(Attr, InnerAcc) ->
		      case proplists:get_value(Attr, A) of
			  undefined ->
			      InnerAcc;
			  Value ->
			      [Value | InnerAcc]
		      end
              end, Acc, Attributes)
    end, [], Modules)).
