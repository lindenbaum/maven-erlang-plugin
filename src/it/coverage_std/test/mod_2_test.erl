-module(mod_2_test).

-include_lib("eunit/include/eunit.hrl").

fun_1_test() ->
  ?assertMatch({ok, fun_1}, mod_2:fun_1()).

fun_2_test() ->
  ?assertMatch({ok, fun_2}, mod_2:fun_2()).

fun_3_test() ->
  not_covered.
