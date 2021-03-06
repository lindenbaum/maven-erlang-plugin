-module(test_app_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test_std/include/test.hrl").
-include_lib("test_std/include/test_include.hrl").

internal_fun_first_test() ->
    ?assertMatch(ok, test_app:internal_fun()).

internal_fun_second_test() ->
    ?assertMatch(ok, test_app:internal_fun()).

define_test() ->
    ?assertMatch(ok, ?TEST_DEFINE).

test_define_test() ->
    ?assertMatch(ok, ?TEST_INCLUDE_DEFINE).

start_stop_test() ->
    ?assertEqual(ok, application:start(test_std)),
    ?assertEqual(ok, application:stop(test_std)).
