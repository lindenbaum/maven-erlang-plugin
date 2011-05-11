package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} that executing and profiles a list of (eunit) tests using
 * {@code eprof}.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @since 2.1.0
 */
public class ProfilingScript extends AbstractScript<ProfilingResult> {
  private final List<File> tests;
  private final File profilingReportsDir;
  private final String profilingReportName;
  private final int timeout;

  public ProfilingScript(List<File> tests, File profilingReportsDir, String profilingReportName, int timeout) throws MojoExecutionException {
    super();
    this.tests = tests;
    this.profilingReportsDir = profilingReportsDir;
    this.profilingReportName = profilingReportName;
    this.timeout = timeout;
  }

  @Override
  public String get() {
    String profilingReportsPath = this.profilingReportsDir.getAbsolutePath();
    String testList = ErlUtils.toModuleList(this.tests, "'", "'");
    return String.format(this.script, profilingReportsPath, this.profilingReportName, testList, this.timeout);
  }

  /**
   * Converts the result of the {@link Script} execution into an object capable
   * of logging the test output as well as returning whether the profiled unit
   * test execution succeeded.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public ProfilingResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    final OtpErlangAtom level = (OtpErlangAtom) resultTuple.elementAt(0);
    final OtpErlangList output = (OtpErlangList) resultTuple.elementAt(1);
    return new ProfilingResult() {
      @Override
      public boolean testsPassed() {
        return !"error".equals(level.atomValue());
      }

      @Override
      public void logOutput(Log log) {
        LogLevel logLevel = LogLevel.fromString(level.atomValue());
        for (int i = 0; i < output.arity(); ++i) {
          MavenUtils.logMultiLineString(log, logLevel, ErlUtils.toString(output.elementAt(i)));
        }
      }
    };
  }
}
