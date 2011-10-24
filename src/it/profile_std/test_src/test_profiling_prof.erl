-module(test_profiling_prof).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
   ?assertMatch({ok, parameter}, test_profiling:test(parameter)).

fib_tail_recursive_test_() ->
    {timeout, 60, ?_assertMatch(28657, test_profiling:tail_recursive_fib(23))},
    {timeout, 60, ?_assertMatch(28657, test_profiling:tail_recursive_fib(23))},
    {timeout, 60, ?_assertMatch(28657, test_profiling:tail_recursive_fib(23))}.

fib_regular_test_() ->
    {timeout, 240, ?_assertMatch(28657, test_profiling:regular_fib(23))},
    {timeout, 240, ?_assertMatch(28657, test_profiling:regular_fib(23))},
    {timeout, 240, ?_assertMatch(28657, test_profiling:regular_fib(23))}.
