-module(x_some_behaviour).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{a, 1},
     {b, 1},
     {c, 1}].
