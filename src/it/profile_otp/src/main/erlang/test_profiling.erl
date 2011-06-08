%%%-------------------------------------------------------------------
%%% @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
%%% @copyright (C) 2010, Olle Törnström
%%% @doc
%%% Very simple module that only has one public function, providing a
%%% base for test profiling.
%%% @end
%%% Created : 19 Apr 2011 by Olle Törnström <olle.toernstroem@lindenbaum.eu> 
%%%-------------------------------------------------------------------
-module(test_profiling).
-export([test/0,
         test/1,
         regular_fib/1,
         tail_recursive_fib/1]).

test() ->
  ok.

test(Arg) ->
    {ok, Arg}.

regular_fib(N) ->
    fib(N).

tail_recursive_fib(N) ->
    fib_tr(N, 0, 1).

%% PRIVATE ---

fib(0) ->
  0;
fib(1) ->
  1;
fib(N) ->
  fib(N - 1) + fib(N - 2).

fib_tr(0, Acc, _Next) ->
    Acc;
fib_tr(N, Acc, Next) ->
    fib_tr(N - 1, Next, Acc + Next).
