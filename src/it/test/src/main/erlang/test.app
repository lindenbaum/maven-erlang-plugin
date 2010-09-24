{application, test,
 [
  {description, "Erlang Test Application"},
  {id, "test"},
  {vsn, ?APP_VERSION},
  {modules, [test_app, test_server]},
  {maxT, infinity},
  {registered, [test_server]},
  {included_applications, []},
  {applications, [kernel, stdlib, sasl]},
  {env, []},
  {mod, {test_app, []}}
 ]}.
