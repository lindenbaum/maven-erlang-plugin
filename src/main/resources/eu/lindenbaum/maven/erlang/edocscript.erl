In = "%s",
Out = [{dir, "%s"}],
Overview = [{overview, "%s"}],
Application = '%s',
Options = [{todo, true}, {new, true}, {subpackages, true}],
edoc:application(Application, In, Out ++ Overview ++ Options).
