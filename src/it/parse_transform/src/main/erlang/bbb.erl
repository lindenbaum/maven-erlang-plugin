-module(bbb).
-export([parse_transform/2]).
parse_transform(Forms, Options) ->
  io:format("forms: ~p~n option: ~p~n", [Forms, Options]),
  Forms.
