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
  NL + "LibDir = code:lib_dir()," + NL + //
      "lists:foreach(" + NL + //
      "  fun({_, preloaded}) ->" + NL + //
      "        ok;" + NL + //
      "     ({Module, Path}) when is_list(Path) ->" + NL + //
      "        case string:str(Path, LibDir) of" + NL + //
      "             1 ->" + NL + //
      "                 ok;" + NL + //
      "             _ ->" + NL + //
      "                 code:purge(Module)," + NL + //
      "                 code:delete(Module)," + NL + //
      "                 code:purge(Module)" + NL + //
      "        end;" + NL + //
      "     ({Module, cover_compiled}) ->" + NL + //
      "        code:purge(Module)," + NL + //
      "        code:delete(Module)," + NL + //
      "        code:purge(Module)" + NL + //
      "  end, code:all_loaded())." + NL;

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
