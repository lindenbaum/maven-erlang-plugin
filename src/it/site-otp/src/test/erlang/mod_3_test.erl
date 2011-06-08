-module(mod_3_test).

-include_lib("eunit/include/eunit.hrl").

fun_1_test() ->
  ?assertMatch({ok, fun_1}, mod_3:fun_1()).

fun_2_test() ->
  ?assertMatch({ok, fun_2}, mod_3:fun_2()).

fun_3_test() ->
  ?assertMatch({ok, fun_3}, mod_3:fun_3()).

-ifdef(TEST).
test_define_test() ->
  ?assert(true).
-else.
test_define_test() ->
  ?assert(false).
-endif.
