package eu.lindenbaum.maven.erlang;

import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} starting a list of erlang applications on a specific node.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class StartApplicationScript extends AbstractScript<StartResult> {
  private final String node;
  private final List<String> applications;

  /**
   * Creates a {@link Script} trying to start a set of erlang applications.
   * 
   * @param node to start the applications on
   * @param applications to start
   */
  public StartApplicationScript(String node, List<String> applications) throws MojoExecutionException {
    super();
    this.node = node;
    this.applications = applications;
  }

  @Override
  public String get() {
    String applications = ErlUtils.toList(this.applications, null, "'", "'");
    return String.format(this.script, this.node, applications);
  }

  /**
   * Converts the result of the {@link Script} execution into an object capable
   * of logging the test output as well as returning whether the unit test
   * execution succeeded.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public StartResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    final String resultMessage = ErlUtils.toString(resultTuple.elementAt(0));
    final OtpErlangList beforeList = (OtpErlangList) resultTuple.elementAt(1);
    return new StartResult() {
      @Override
      public boolean startSucceeded() {
        return "ok".equals(resultMessage);
      }

      @Override
      public void logError(Log log) {
        if (!startSucceeded()) {
          MavenUtils.logMultiLineString(log, LogLevel.ERROR, resultMessage);
        }
      }

      @Override
      public List<String> getBeforeApplications() {
        ArrayList<String> resultList = new ArrayList<String>();
        for (int i = 0; i < beforeList.arity(); ++i) {
          resultList.add(ErlUtils.toString(beforeList.elementAt(i)));
        }
        return resultList;
      }
    };
  }
}
