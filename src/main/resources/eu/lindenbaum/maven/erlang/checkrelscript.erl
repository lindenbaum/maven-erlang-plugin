RelFile = "%s",
case file:consult(RelFile) of
    {ok, [{release, {N, V}, {erts, E}, Apps}]} ->
        {ok, N, V, E, lists:map(
			fun({App, Vsn}) -> {App, Vsn};
			   ({App, Vsn, _}) -> {App, Vsn};
			   ({App, Vsn, _, _}) -> {App, Vsn}
			end, Apps)};
    _ ->
        {error, undefined, undefined, undefined, []}
end.
