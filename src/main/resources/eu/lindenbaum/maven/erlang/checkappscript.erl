AppFile = "%s",
case file:consult(AppFile) of
    {ok, [{application, A, Props}]} ->
        V = proplists:get_value(vsn, Props, undefined),
        S = proplists:get_value(mod, Props, omitted),
        M = proplists:get_value(modules, Props, []),
        D = proplists:get_value(applications, Props, []),
        case S of
	    {Module, _} -> {ok, A, V, Module, M, D};
	    _ -> {ok, A, V, S, M, D}
	end;
    _ ->
        {error, undefined, undefined, undefined, [], []}
end.
