-module(mod_6).

-export([exported_tested/0,
         exported_not_tested/0]).

exported_tested() ->
  ok.
  
not_exported_tested() ->
  ok.
  
exported_not_tested() ->
  ok.
  
not_exported_not_tested() ->
  ok.
