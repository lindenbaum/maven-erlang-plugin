-module(test_server_test).

-include_lib("eunit/include/eunit.hrl").

current_time_millis_test() ->
    Millis = test_server:current_time_millis(),
    ?assert(Millis > 0),
    ?assert(Millis < test_server:current_time_millis()).

test_round2_test() ->
    ?assertMatch(ok, test_server:test_round(0, 0)).

priv_dir_test() ->
    case code:priv_dir(package_otp) of
        {error, _} -> ?assert(false);
        _ -> ok
    end.

