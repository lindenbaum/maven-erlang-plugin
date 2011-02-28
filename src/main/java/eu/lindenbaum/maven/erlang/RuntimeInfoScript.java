package eu.lindenbaum.maven.erlang;

import java.io.File;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * A {@link Script} that can be used to retrieve runtime information from the
 * plugins backend node.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class RuntimeInfoScript implements Script<RuntimeInfo> {
  private static final String script = //
  NL + "{code:lib_dir()," + NL + //
      "code:root_dir()," + NL + //
      "erlang:system_info(version)," + NL + //
      "erlang:system_info(otp_release)}." + NL;

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
      public File getRootDirectory() {
        return new File(ErlUtils.toString(resultTuple.elementAt(1)));
      }

      @Override
      public String getVersion() {
        return ErlUtils.toString(resultTuple.elementAt(2));
      }

      @Override
      public String getOtpRelease() {
        return ErlUtils.toString(resultTuple.elementAt(3));
      }
    };
  }
}
