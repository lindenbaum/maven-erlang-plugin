%%%-------------------------------------------------------------------
%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%% @doc
%%%  An {@link eunit_listener} collecting/writing surefire compatible
%%%  reports.
%%% @end
%%% Created : 1 Oct 2010
%%%-------------------------------------------------------------------
-module(surefire).
-author('Tobias Schlager').

-behaviour(eunit_listener).

-export([start/0, start/1, init/1, handle_begin/3, handle_end/3,
	 handle_cancel/3, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% define section

-define(DEFAULT_SUITE, "Suite").
-define(DEFAULT_OUTPUT_DIR, ".").
-define(DEFAULT_TEST, {unknown, unknown, 0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% record section

-record(state, {
	  output_dir = ?DEFAULT_OUTPUT_DIR,
	  package = "",
	  suites = dict:new()}).

-record(suite, {
	  data = [],
	  cases = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function section

%-------------------------------------------------------------------------------
% Initialize this module.
%-------------------------------------------------------------------------------
start() ->
    start([]).

%-------------------------------------------------------------------------------
% Initialize this module with options.
%-------------------------------------------------------------------------------
start(Options) ->
    eunit_listener:start(?MODULE, Options).

%-------------------------------------------------------------------------------
% Initialize this module with options. Default output path is
% {@code ?DEFAULT_OUTPUT_DIR}.
%-------------------------------------------------------------------------------
init(Options) ->
    Dir = proplists:get_value(dir, Options, ?DEFAULT_OUTPUT_DIR),
    Package = proplists:get_value(package, Options, ""),
    #state{output_dir = Dir, package = Package}.

%-------------------------------------------------------------------------------
% Handles the begin of a test case or suite.
%-------------------------------------------------------------------------------
handle_begin(group, Data, State) ->
    case get_suite_id(Data) of
	undefined ->
	    State;
	SuiteId ->
	    store_suite(SuiteId, Data, State)
    end;
handle_begin(_, _, State) ->
    State.

%-------------------------------------------------------------------------------
% Handles the end of a test case or suite.
%-------------------------------------------------------------------------------
handle_end(test, Data, State) ->
    case get_test_id(Data) of
	undefined ->
	    State;
	{SuiteId, _} ->
	    store_test(SuiteId, Data, State)
    end;
handle_end(group, Data, State) ->
    case get_suite_id(Data) of
	undefined ->
	    State;
	SuiteId ->
	    store_suite(SuiteId, Data, State)
    end;
handle_end(_, _, State) ->
    State.

%-------------------------------------------------------------------------------
% Handles the cancellation of a test case or suite.
%-------------------------------------------------------------------------------
handle_cancel(test, Data, State) ->
    case get_test_id(Data) of
	undefined ->
	    State;
	{SuiteId, _} ->
	    Reason = proplists:get_value(reason, Data),
	    store_test(SuiteId, [{status, {skipped, Reason}}] ++ Data, State)
    end;
handle_cancel(group, Data, State) ->
    case get_suite_id(Data) of
	undefined ->
	    State;
	SuiteId ->
	    Reason = proplists:get_value(reason, Data),
	    store_suite(SuiteId, [{status, {skipped, Reason}}] ++ Data, State)
    end;
handle_cancel(_, _, State) ->
    State.

%-------------------------------------------------------------------------------
% Writes the test report xml files.
%-------------------------------------------------------------------------------
terminate(_, #state{output_dir = OutputDir, package = P, suites = Suites}) ->
    dict:map(
      fun(_, #suite{data = Data, cases = Cases}) ->
	      FileName = filename:join([OutputDir, get_suite_filename(Data)]),
	      SuiteReport = get_suite_report(P, Data, Cases),
	      ok = file:write_file(FileName, SuiteReport, [{encoding, utf8}])
      end, Suites).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal function section

%-------------------------------------------------------------------------------
% Writes a suite with its data into the given state. In case the suite
% already exists only the data field will be updated by adding the new data.
%-------------------------------------------------------------------------------
store_suite(SuiteId, Data, State = #state{suites = Suites}) ->
    Suite = case dict:find(SuiteId, Suites) of
		{ok, S = #suite{data = OldData}} ->
		    S#suite{data = Data ++ OldData};
		_ ->
		    #suite{data = Data}
	    end,
    Suites2 = dict:store(SuiteId, Suite, Suites),
    State#state{suites = Suites2}.

%-------------------------------------------------------------------------------
% Writes a test case into a specific suite of the given state. In case the
% test case already exists the state is not changed. In case the according
% suite does not exist a default suite will be created.
%-------------------------------------------------------------------------------
store_test(SuiteId, Data, State = #state{suites = Suites}) ->
    case dict:find(SuiteId, Suites) of
	{ok, Suite = #suite{cases = Cases}} ->
	    Suite2 = Suite#suite{cases = [Data | Cases]},
	    Suites2 = dict:store(SuiteId, Suite2, Suites),
	    State#state{suites = Suites2};
	_ ->
	    NewState = store_suite(SuiteId, [], State),
	    store_test(SuiteId, Data, NewState)
    end.

%-------------------------------------------------------------------------------
% Extracts the suite id from the given data.
%-------------------------------------------------------------------------------
get_suite_id(Data) ->
    case proplists:get_value(id, Data) of
	[SuiteId | _] ->
	    SuiteId;
	_ ->
	    undefined
    end.

%-------------------------------------------------------------------------------
% Extracts the suite and test id from the given data.
%-------------------------------------------------------------------------------
get_test_id(Data) ->
    case proplists:get_value(id, Data) of
	[SuiteId, TestId | _] ->
	    {SuiteId, TestId};
	_ ->
	    undefined
    end.

%-------------------------------------------------------------------------------
% Returns how many cases contain the requested status from the given test case
% list.
%-------------------------------------------------------------------------------
get_number_for_status(Category, Cases) ->
    lists:foldl(
      fun(Case, Acc) ->
	      case proplists:get_value(status, Case) of
		  {Category, _} -> Acc + 1;
		  Category -> Acc + 1;
		  _ -> Acc
	      end
      end, 0, Cases).

%-------------------------------------------------------------------------------
% Returns a string containing the report of a certain test suite.
%-------------------------------------------------------------------------------
get_suite_report(Package, Data, Cases) ->
    Failed = get_number_for_status(error, Cases),
    Skipped = get_number_for_status(skipped, Cases),
    Canceled = get_number_for_status(cancel, Cases),
    Size = proplists:get_value(size, Data, 0),
    Time = proplists:get_value(time, Data, 0),
    Name = get_suite_name(proplists:get_value(desc, Data, ?DEFAULT_SUITE)),
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" ++
	io_lib:format("<testsuite failures=\"~p\"", [Failed]) ++
	io_lib:format(" time=\"~p\"", [Time]) ++
	io_lib:format(" errors=\"~p\"", [Canceled]) ++
	io_lib:format(" skipped=\"~p\"", [Skipped]) ++
	io_lib:format(" tests=\"~p\"", [Size]) ++
	io_lib:format(" name=\"~s~s\">\n", [Package, Name]) ++
	lists:foldr(fun(Case, Acc) ->
			    Acc ++ get_test_report(Package, Case)
		    end, "", Cases) ++
	"</testsuite>\n".

%-------------------------------------------------------------------------------
% Returns a string containing the report of a certain test case.
%-------------------------------------------------------------------------------
get_test_report(Package, Case) ->
    Time = proplists:get_value(time, Case, 0),
    {M, F, A} = proplists:get_value(source, Case, ?DEFAULT_TEST),
    io_lib:format("  <testcase time=\"~p\"", [Time]) ++
	io_lib:format(" classname=\"~s~p\"", [Package, M]) ++
	io_lib:format(" name=\"~p~p\">\n", [F, A]) ++
	case proplists:get_value(status, Case) of
	    {Result, Reason} when Result == skipped; Result == cancel ->
		"    <failure type=\"" ++
		    unline_string(io_lib:format("~p", [Result])) ++
		    "\" message=\"" ++
		    unline_string(io_lib:format("~p", [Reason])) ++
		    "\">\n    </failure>\n";
	    {error, {error, {Type, Msg}, Trace}} ->
		"    <failure type=\"" ++
		    unline_string(io_lib:format("~p", [Type])) ++
		    "\" message=\"" ++
		    unline_string(io_lib:format("~p", [Msg])) ++
		    "\">\n" ++
		    escape_string(io_lib:format("~p\n", [Trace])) ++
		    "    </failure>\n";
	    {error, {error, Type, Trace}} ->
		"    <failure type=\"" ++
		    unline_string(io_lib:format("~p", [Type])) ++
		    "\">\n" ++
		    escape_string(io_lib:format("~p\n", [Trace])) ++
		    "    </failure>\n";
	    _ ->
		""
	end ++ "  </testcase>\n".

%-------------------------------------------------------------------------------
% Return a valid suite name parseable by the surefire-report-plugin.
%-------------------------------------------------------------------------------
get_suite_name(Name) ->
    string_replace(string_replace(binary_to_list(Name), "module ", ""), "'", "").

%-------------------------------------------------------------------------------
% Returns a surefire-report-plugin recognizable test suite file name.
%-------------------------------------------------------------------------------
get_suite_filename(Data) ->
    Name = proplists:get_value(desc, Data, ?DEFAULT_SUITE),
    "TEST-" ++ get_suite_name(Name) ++ ".xml".

%-------------------------------------------------------------------------------
% Parses a string and escaping it for xml output.
%-------------------------------------------------------------------------------
escape_string(String) ->
    escape_string(lists:flatten(String), [], false).
escape_string(String, Unline) ->
    escape_string(lists:flatten(String), [], Unline).
escape_string([], Acc, _) ->
    lists:reverse(Acc);
escape_string([$\n | Tail], Acc, true) ->
    escape_string(Tail, Acc, true);
escape_string([$\r | Tail], Acc, true) ->
    escape_string(Tail, Acc, true);
escape_string([$< | Tail], Acc, Unline) ->
    escape_string(Tail, [$;, $t, $l, $& | Acc], Unline);
escape_string([$> | Tail], Acc, Unline) ->
    escape_string(Tail, [$;, $t, $g, $& | Acc], Unline);
escape_string([$& | Tail], Acc, Unline) ->
    escape_string(Tail, [$;, $p, $m, $a, $& | Acc], Unline);
escape_string([$" | Tail], Acc, Unline) ->
    escape_string(Tail, [$;, $t, $o, $u, $q, $& | Acc], Unline);
escape_string([Char | Tail], Acc, Unline) when is_integer(Char) ->
    escape_string(Tail, [Char | Acc], Unline).

%-------------------------------------------------------------------------------
% Parses a string escaping it for xml output as well as removing line breaks.
%-------------------------------------------------------------------------------
unline_string(String) ->
    escape_string(String, true).

%-------------------------------------------------------------------------------
% Replaces all occurences of a (sub)string in a string with another string.
%-------------------------------------------------------------------------------
string_replace(String, ToReplace, ReplaceWith) ->
    string_replace(String, ToReplace, ReplaceWith, "").
string_replace(Rest, ToReplace, ReplaceWith, Acc) ->
    Len = string:len(Rest),
    ToReplaceLen = string:len(ToReplace),
    case string:str(Rest, ToReplace) of
	0 ->
	    Acc ++ Rest;
	Start ->
	    string_replace(
	      string:right(Rest, Len - ToReplaceLen - Start + 1),
	      ToReplace,
	      ReplaceWith,
	      Acc ++ string:left(Rest, Start - 1) ++ ReplaceWith)
    end.
