LibDir = code:lib_dir(),
lists:foreach(
  fun({_, preloaded}) ->		 
	  ok;
     ({M, _}) when M =:= distel
                   orelse M =:= distel_ie
                   orelse M =:= fdoc
                   orelse M =:= otp_doc ->
	  ok;
     ({Module, Path}) when is_list(Path) ->
	  case string:str(Path, LibDir) of
	      1 ->
		  ok;
	      _ ->
		  code:purge(Module),
		  code:delete(Module),
		  code:purge(Module)
	  end;
     ({Module, cover_compiled}) ->
	  code:purge(Module),
	  code:delete(Module),
	  code:purge(Module)
  end,
  code:all_loaded()).
