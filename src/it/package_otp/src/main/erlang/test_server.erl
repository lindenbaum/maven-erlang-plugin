%%%-------------------------------------------------------------------
%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%% @copyright (C) 2010, Tobias Schlager
%%% @doc
%%% Server that starts a test run upon receipt of a specific message.
%%% @end
%%% Created : 14 Sep 2010 by Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%%-------------------------------------------------------------------
-module(test_server).

-behaviour(gen_server).

-registered([?MODULE]).

%% API
-export([start_link/0, test/1, current_time_millis/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid::pid()} | ignore | {error, Error::term()}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts an asynchonous test round on this server
%%
%% @spec test(Iterations::integer()) -> ok
%% @end
%%--------------------------------------------------------------------
test(Iterations) ->
    gen_server:cast(?MODULE, {test, Iterations}).

%%--------------------------------------------------------------------
%% @doc
%% Returns the number of milliseconds since the epoch using
%% {@link erlang:now/0}.
%%
%% @spec current_time_millis() -> integer()
%% @end
%%--------------------------------------------------------------------
current_time_millis() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs / 1000.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(nil()) -> {ok, State::state()}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Msg::term(), pid(), State::state()) ->
%%           {reply, ok, State::state()}
%% @end
%%--------------------------------------------------------------------
handle_call(Msg, _, State) ->
    error_logger:info_msg("received unhandled message ~p~n", [Msg]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(term(), State::state()) -> {noreply, State::state()}
%% @end
%%--------------------------------------------------------------------
handle_cast({test, Iterations}, State) ->
    test_round(Iterations),
    {noreply, State};
handle_cast(Msg, State) ->
    error_logger:info_msg("received unhandled message ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(term(), State::state()) -> {noreply, State::state()}
%% @end
%%--------------------------------------------------------------------
handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(term(), term()) -> ok
%% @end
%%--------------------------------------------------------------------
terminate(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_round(Iterations) ->
    error_logger:info_msg("starting test round with ~p iterations~n", [Iterations]),
    S = current_time_millis(),
    test_round(Iterations, 0),
    E = current_time_millis(),
    error_logger:info_msg("test round with ~p iterations took ~p ms~n", [Iterations, E - S]).

test_round(0, _) ->
    ok;
test_round(N, C) ->
    error_logger:info_msg("~512c~n", [$0 + C]),
    test_round(N - 1, (C + 1) rem 10).
