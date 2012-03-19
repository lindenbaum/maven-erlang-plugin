-module(app_1_mod_1).

start() ->
  app_2_mod_1:start(),
  ok.

stop() ->
  ok.