-module(test_module).

-include("test_include.hrl").

-export([test_function/0]).

test_function() ->
    ?TEST_DEFINE.
