-module(mod_1_test).

-include_lib("eunit/include/eunit.hrl").

fun_1_test() ->
  ?assertMatch({ok, fun_1}, mod_1:fun_1()).

fun_2_test() ->
  not_covered.

fun_3_test() ->
  not_covered.
