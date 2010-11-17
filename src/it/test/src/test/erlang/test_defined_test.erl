-module(test_defined_test).
-include_lib("eunit/include/eunit.hrl").

ensure_test_is_defined_test() ->
  ?assertMatch(test_is_defined, test_defined:is_defined()).
