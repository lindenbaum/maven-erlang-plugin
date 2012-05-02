-module(z_some_behaviour).
-export([some_function/0]).

-callback a(any()) -> ok.
-callback b(any()) -> ok.
-callback c(any()) -> ok.

some_function() ->
  ok.
