package eu.lindenbaum.maven.erlang;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} that can be used to retrieve runtime information from the
 * plugins backend node.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class RuntimeInfoScript implements Script<RuntimeInfo> {
  private static final String script = "{code:lib_dir(), erlang:system_info(version)}.";

  public RuntimeInfoScript() {
    // ignored
  }

  @Override
  public String get() {
    return script;
  }

  /**
   * Converts the result of the {@link Script} execution into an
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public RuntimeInfo handle(final OtpErlangObject result) {
    final OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    return new RuntimeInfo() {
      @Override
      public File getLibDirectory() {
        return new File(ErlUtils.toString(resultTuple.elementAt(0)));
      }

      @Override
      public String getVersion() {
        return ErlUtils.toString(resultTuple.elementAt(1));
      }
    };
  }
}
