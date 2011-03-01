package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that loads a list of modules located in the current code
 * paths. All modules will be purged before loading to prevent module version
 * conflicts.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class LoadModulesScript extends AbstractScript<Integer> {
  private final List<File> modules;

  public LoadModulesScript(List<File> modules) throws MojoExecutionException {
    super();
    this.modules = modules;
  }

  @Override
  public String get() {
    String modules = ErlUtils.toModuleList(this.modules, "'", "'");
    return String.format(this.script, modules);
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
