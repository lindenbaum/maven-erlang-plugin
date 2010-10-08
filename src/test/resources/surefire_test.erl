-module(surefire_test).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_test() ->
    ?assertMatch({state, ".", "", _}, surefire:init([])),
    ?assertMatch({state, ".", "la", _}, surefire:init([{package, "la"}])),
    ?assertMatch({state, "lala", "", _}, surefire:init([{dir, "lala"}])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_begin_0_test() ->
    Data = [{id, [1]}],
    State = {state, "", "", dict:new()},
    {state, "", "", Dict} = surefire:handle_begin(group, Data, State),
    ?assertMatch({suite, Data, []}, dict:fetch(1, Dict)).

handle_begin_1_test() ->
    Data = [{id, [1, 2, 3]}],
    State = {state, "", "", dict:new()},
    {state, "", "", Dict} = surefire:handle_begin(group, Data, State),
    ?assertMatch({suite, Data, []}, dict:fetch(1, Dict)).

handle_begin_2_test() ->
    ?assertMatch(state, surefire:handle_begin(group, [], state)).

handle_begin_3_test() ->
    ?assertMatch(state, surefire:handle_begin(test, [], state)).

handle_begin_4_test() ->
    ?assertMatch(state, surefire:handle_begin(lala, ignored, state)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_end_0_test() ->
    Data = [{id, [1]}],
    State = {state, "", "", dict:new()},
    Next = surefire:handle_begin(group, Data, State),
    {state, "", "", Dict} = surefire:handle_end(group, [update] ++ Data, Next),
    NewData = [update] ++ Data ++ Data,
    ?assertMatch({suite, NewData, []}, dict:fetch(1, Dict)).

handle_end_1_test() ->
    Data = [{id, [1, 2, 3]}],
    State = {state, "", "", dict:new()},
    Next = surefire:handle_begin(test, Data, State),
    NewData = [update] ++ Data,
    {state, "", "", Dict} = surefire:handle_end(test, NewData, Next),
    ?assertMatch({suite, [], [NewData]}, dict:fetch(1, Dict)).

handle_end_2_test() ->
    ?assertMatch(state, surefire:handle_end(group, [], state)).

handle_end_3_test() ->
    ?assertMatch(state, surefire:handle_end(test, [], state)).

handle_end_4_test() ->
    ?assertMatch(state, surefire:handle_end(lala, ignored, state)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cancel_0_test() ->
    Data = [{id, [1]}],
    State = {state, "", "", dict:new()},
    Next = surefire:handle_begin(group, Data, State),
    {state, "", "", Dict} = surefire:handle_cancel(group, Data, Next),
    NewData = [{status, {skipped, undefined}}] ++ Data ++ Data,
    ?assertMatch({suite, NewData, []}, dict:fetch(1, Dict)).

handle_cancel_1_test() ->
    Data = [{id, [1, 2, 3]}],
    State = {state, "", "", dict:new()},
    Next = surefire:handle_begin(test, Data, State),
    {state, "", "", Dict} = surefire:handle_cancel(test, Data, Next),
    NewData = [{status, {skipped, undefined}}] ++ Data,
    ?assertMatch({suite, [], [NewData]}, dict:fetch(1, Dict)).

handle_cancel_2_test() ->
    ?assertMatch(state, surefire:handle_cancel(group, [], state)).

handle_cancel_3_test() ->
    ?assertMatch(state, surefire:handle_cancel(test, [], state)).

handle_cancel_4_test() ->
    ?assertMatch(state, surefire:handle_cancel(lala, ignored, state)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate_0_test() ->
    surefire:terminate(ignored, {state, ".", "", dict:new()}),
    {ok, Files} = file:list_dir("."),
    lists:foreach(fun(E) -> ?assertMatch(0, string:str(E, "TEST-")) end, Files).

terminate_1_test() ->
    Expected =
	"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" ++
	"<testsuite failures=\"1\" time=\"1\" errors=\"0\" " ++
	"skipped=\"0\" tests=\"2\" name=\"test\">\n" ++
	"  <testcase time=\"2\" classname=\"m2\" name=\"f2a2\">\n" ++
	"    <failure type=\"type\" message=\"msg\">\n" ++
	"trace\n" ++
	"    </failure>\n" ++
	"  </testcase>\n" ++
	"  <testcase time=\"1\" classname=\"m1\" name=\"f1a1\">\n" ++
	"  </testcase>\n" ++
	"</testsuite>\n",

    Case1 = [{id, [1, 1]}, {time, 1}, {source, {m1,f1,a1}}, {status, ok}],
    Case2 = [{id, [1, 2]}, {time, 2}, {source, {m2,f2,a2}},
	     {status, {error, {error, {type, msg}, trace}}}],
    Suite = [{id, [1]}, {time, 1}, {desc, <<"module 'test'">>}, {size, 2}],
    Dict = dict:store(1, {suite, Suite, [Case1, Case2]}, dict:new()),
    surefire:terminate(ignored, {state, ".", "", Dict}),
    {ok, Binary} = file:read_file("TEST-test.xml"),
    ?assertMatch(Expected, binary_to_list(Binary)),
    file:delete("TEST-test.xml").
