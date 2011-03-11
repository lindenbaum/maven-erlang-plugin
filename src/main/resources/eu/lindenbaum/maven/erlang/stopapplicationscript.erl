ToPreserve = %s,
ToStop = [A || {A, _, _} <- application:which_applications()] -- ToPreserve,
[begin application:stop(A), application:unload(A) end || A <- ToStop].
