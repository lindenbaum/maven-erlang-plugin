%%%-------------------------------------------------------------------
%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%% @copyright (C) 2010, Lindenbaum GmbH
%%% @doc
%%% Application callback for the Erlang test application. Starts
%%% the main server of this application using {@link test_server:start_link/0}.
%%% @see test_server
%%% @end
%%% Created : 14 Sep 2010 by Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%%-------------------------------------------------------------------
-module(test_app).

-behaviour(application).

-include("../include/test.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(term(), term()) -> {ok, Pid} | {ok, Pid, State} |
%%                                {error, Reason}
%% @end
%%--------------------------------------------------------------------
start(_, _) ->
    case test_server:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(term()) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

internal_fun() ->
    ?TEST_DEFINE.
