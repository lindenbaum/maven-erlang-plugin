-module(aaa).
-export([foobar/0]).
-compile({parse_transform, bbb}).
foobar() ->
  ok.