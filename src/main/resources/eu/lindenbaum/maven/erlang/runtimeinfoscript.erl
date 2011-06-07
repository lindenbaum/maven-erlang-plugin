{filename:join([code:lib_dir()]),
 filename:join([code:root_dir()]),
 erlang:system_info(version),
 erlang:system_info(otp_release),
 [filename:join([P]) || P <- code:get_path(), P =/= "."]}.
