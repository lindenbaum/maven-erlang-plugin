-module(mod_6_test).

-include_lib("eunit/include/eunit.hrl").

exported_tested_test() ->
  ?assertEqual(ok, mod_6:exported_tested()).
  
not_exported_tested_test() ->
  ?assertEqual(ok, mod_6:not_exported_tested()).
