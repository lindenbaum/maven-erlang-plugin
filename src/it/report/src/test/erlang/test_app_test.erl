-module(test_app_test).

-include_lib("eunit/include/eunit.hrl").

internal_fun_first_test() ->
    ?assertMatch(ok, test_app:internal_fun()).

internal_fun_second_test() ->
    ?assertMatch(ok, test_app:internal_fun()).
