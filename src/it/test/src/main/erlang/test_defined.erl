-module(test_defined).
-compile([export_all]).

-ifdef(TEST).
-define(is_test_defined, test_is_defined).
-else.
-define(is_test_defined, test_is_not_defined).
-endif.

is_defined() ->
  ?is_test_defined.
