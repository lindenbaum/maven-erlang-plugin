-module(aaa_test).
-include_lib("eunit/include/eunit.hrl").
ensure_transformed_test() ->
    ?assertMatch(foobar, aaa:foobar()).
