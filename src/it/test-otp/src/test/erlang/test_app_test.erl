-module(test_app_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test_otp/include/test.hrl").
-include_lib("test_otp/include/test_include.hrl").

internal_fun_first_test() ->
    ?assertMatch(ok, test_app:internal_fun()).

internal_fun_second_test() ->
    ?assertMatch(ok, test_app:internal_fun()).

define_test() ->
  ?assertMatch(ok, ?TEST_DEFINE).
  
test_define_test() ->
  ?assertMatch(ok, ?TEST_INCLUDE_DEFINE).