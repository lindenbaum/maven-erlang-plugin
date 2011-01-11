package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} that loads a list of modules located in the provided code
 * paths. All modules will be purged before loading to prevent module version
 * conflicts. The provided code paths will be removed from the backend path
 * after module loading.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class LoadModulesScript implements Script<Integer> {
  private static final String script = //
  NL + "CodePaths = %s," + NL + //
      "Modules = %s," + NL + //
      "code:add_pathsa(CodePaths)," + NL + //
      "L = lists:foldl(" + NL + //
      "      fun(Module, Acc) ->" + NL + //
      "            code:purge(Module)," + NL + //
      "            code:delete(Module)," + NL + //
      "            code:purge(Module)," + NL + //
      "            case code:load_file(Module) of" + NL + //
      "                {module, _} -> Acc + 1;" + NL + //
      "                _ -> Acc" + NL + //
      "            end" + NL + //
      "      end, 0, Modules)," + NL + //
      "[code:del_path(P) || P <- CodePaths]," + NL + //
      "L." + NL;

  private final List<File> modules;
  private final List<File> codePaths;

  public LoadModulesScript(List<File> modules, List<File> codePaths) {
    this.modules = modules;
    this.codePaths = codePaths;
  }

  @Override
  public String get() {
    String paths = ErlUtils.toFileList(this.codePaths, "\"", "\"");
    String modules = ErlUtils.toModuleList(this.modules, "'", "'");
    return String.format(script, paths, modules);
  }

  /**
   * The result of the {@link Script} execution will be converted into an
   * {@link Integer} indicating how many modules were actually loaded.
   * 
   * @return The number of loaded modules, {@code -1} on conversion errors.
   */
  @Override
  public Integer handle(OtpErlangObject result) {
    try {
      return ((OtpErlangLong) result).intValue();
    }
    catch (OtpErlangRangeException e) {
      return -1;
    }
  }
}
