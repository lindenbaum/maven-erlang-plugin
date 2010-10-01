-module(surefire).

-behaviour(eunit_listener).

-export([start/0,
	 start/1,
	 init/1,
	 handle_begin/3,
	 handle_end/3,
	 handle_cancel/3,
	 terminate/2]).

start() ->
    io:format("start()~n"),
    start([]).

start(Options) ->
    io:format("start(~p)~n", [Options]),
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    io:format("init(~p)~n", [Options]).

handle_begin(Type, Data, State) ->
    io:format("handle_begin(~p, ~p, ~p)~n", [Type, Data, State]).
    
handle_end(Type, Data, State) ->
    io:format("handle_end(~p, ~p, ~p)~n", [Type, Data, State]).

handle_cancel(Type, Data, State) ->
    io:format("handle_cancel(~p, ~p, ~p)~n", [Type, Data, State]).

terminate(Reason, State) ->
    io:format("terminate(~p, ~p)~n", [Reason, State]).
