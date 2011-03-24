package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that can be used to retrieve runtime information from the
 * plugins backend node.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class RuntimeInfoScript extends AbstractScript<RuntimeInfo> {
  public RuntimeInfoScript() throws MojoExecutionException {
    super();
  }

  @Override
  public String get() {
    return this.script;
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

    final List<File> paths = new ArrayList<File>();
    OtpErlangList pathList = (OtpErlangList) resultTuple.elementAt(4);
    for (int i = 0; i < pathList.arity(); ++i) {
      paths.add(new File(ErlUtils.toString(pathList.elementAt(i))));
    }

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

      @Override
      public List<File> getPaths() {
        return paths;
      }
    };
  }
}
