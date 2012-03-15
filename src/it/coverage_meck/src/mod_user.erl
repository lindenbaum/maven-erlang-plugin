-module(mod_user).

-export([foobar/0]).

foobar() ->
    Foo = mod_collaborator:foo(),
    Bar = mod_collaborator:bar(),
    Baz = mod_collaborator:baz(),
    Foobar = foobar,
    {ok, [Foo, Bar, Baz, Foobar]}.
