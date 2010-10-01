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
  Pid = self(),
  {ok, touch_file_and_exit(Pid)}. 

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
    c:q(),
    ok.

touch_file_and_exit(Pid) ->
  spawn(fun() ->
    io:format("Creating touch file...~n"),
    file:write_file("touched", <<$t,$o,$u,$c,$h,$e,$d>>),
    io:format("Ending process...~n"),
    exit(Pid, normal)
  end).
