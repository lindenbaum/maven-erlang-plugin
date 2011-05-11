-module(test_profiling_test).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
   ?assertMatch({ok, parameter}, test_profiling:test(parameter)).

fib_tail_recursive_test_() ->
    {timeout, 60, ?_assertMatch(267914296, test_profiling:tail_recursive_fib(42))}.

fib_regular_test_() ->
    {timeout, 240, ?_assertMatch(267914296, test_profiling:regular_fib(42))}.
