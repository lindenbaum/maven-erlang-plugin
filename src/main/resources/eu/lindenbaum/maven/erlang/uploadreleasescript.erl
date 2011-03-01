Node = %s,
Release = "%s",
case net_kernel:connect(Node) of
    true ->
        FileName = filename:basename(Release),
        DestRoot = rpc:call(Node, code, root_dir, []),
        Dest = rpc:call(
                 Node, filename, join,
                 [[DestRoot, "releases", FileName]]),
        case file:read_file(Release) of
            {ok, Binary} ->
                rpc:call(Node, file, write_file, [Dest, Binary]);
            Other ->
                Other
        end;
    false ->
        {error, {cannot_connect, Node}}
end.
