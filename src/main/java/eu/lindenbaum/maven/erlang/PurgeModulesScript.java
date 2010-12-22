package eu.lindenbaum.maven.erlang;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * A {@link Script} that purges all modules currently loaded except the ones
 * loaded directly from the backends lib directory retrieved using
 * <code>code:lib_dir/0</code>.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class PurgeModulesScript implements Script<Void> {
  private static final String script = //
  "    LibDir = code:lib_dir()," + //
      "lists:foreach(" + //
      "  fun({_, preloaded}) ->" + //
      "        ok;" + //
      "     ({Module, Path}) when is_list(Path) ->" + //
      "        case string:str(Path, LibDir) of" + //
      "             1 ->" + //
      "                 ok;" + //
      "             _ ->" + //
      "                 code:purge(Module)," + //
      "                 code:delete(Module)," + //
      "                 code:purge(Module)" + //
      "        end;" + //
      "     ({Module, cover_compiled}) ->" + //
      "        code:purge(Module)," + //
      "        code:delete(Module)," + //
      "        code:purge(Module)" + //
      "  end, code:all_loaded()).";

  public PurgeModulesScript() {
    // no params
  }

  @Override
  public String get() {
    return script;
  }

  /**
   * The result of the {@link Script} execution is ignored.
   */
  @Override
  public Void handle(OtpErlangObject result) {
    return null;
  }
}
