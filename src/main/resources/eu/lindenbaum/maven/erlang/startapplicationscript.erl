Node = %s,
Applications = %s,
Which = rpc:call(Node, application, which_applications, []),
Before = [A || {A, _, _} <- Which],
Fun = fun([], ok, _) ->
              ok;
         ([A | Rest], ok, Rec) ->
              case rpc:call(Node, application, start, [A]) of
                  ok ->
                      Rec(Rest, ok, Rec);
                  {error, {already_started, _}} ->
                      Rec(Rest, ok, Rec);
                  {error, {not_started, Dep}} ->
                      Rec([Dep, A] ++ Rest, ok, Rec);
                  {error, {"no such file or directory", AppFile}} ->
                      "could not find application file \"" ++ AppFile ++ "\"\n"
                      ++ "maybe dependencies are missing, try a re-run"
                      ++" using -DwithDependencies";
                  Error ->
                      Error
              end;
         (_, Error, _) ->
              Error
      end,
{Fun(Applications, ok, Fun), Before}.
