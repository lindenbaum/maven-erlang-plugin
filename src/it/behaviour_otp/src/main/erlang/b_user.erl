-module(b_user).

-behaviour(x_some_behaviour).

-export([a/1, b/1, c/1]).

a(foo) -> ok.
b(bar) -> ok.
c(baz) -> ok.
