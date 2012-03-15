-module(mod_user_test).

-include_lib("eunit/include/eunit.hrl").

mod_user_test() ->
    ok = meck:new(mod_collaborator),
    ok = meck:expect(mod_collaborator, foo, fun() -> meck_foo end),
    ok = meck:expect(mod_collaborator, bar, fun() -> meck_bar end),
    ok = meck:expect(mod_collaborator, baz, fun() -> meck_baz end),
    ?assertMatch({ok, [meck_foo, meck_bar, meck_baz, foobar]}, mod_user:foobar()),
    ?assertMatch(true, meck:validate(mod_collaborator)),
    ok = meck:unload(mod_collaborator),
    ?assertMatch({ok, [foo, bar, baz, foobar]}, mod_user:foobar()),
    ok.
  
