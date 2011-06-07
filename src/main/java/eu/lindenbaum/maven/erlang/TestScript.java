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
 * A {@link Script} executing a list of (eunit) tests.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class TestScript extends AbstractScript<TestResult> {
  private final List<File> tests;
  private final File surefireDir;
  private final String suiteName;

  /**
   * Creates a {@link Script} executing a list of eunit tests.
   * 
   * @param tests to run
   * @param surefireDir to output surefire compatible reports into
   * @param suiteName the name of the test suite (for surefire)
   */
  public TestScript(List<File> tests, File surefireDir, String suiteName) throws MojoExecutionException {
    super();
    this.tests = tests;
    this.surefireDir = surefireDir;
    this.suiteName = suiteName;
  }

  @Override
  public String get() {
    String surefirePath = this.surefireDir.getAbsolutePath();
    String testList = ErlUtils.toModuleList(this.tests, "'", "'");
    return String.format(this.script, surefirePath, this.suiteName, testList);
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
  public TestResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    final OtpErlangAtom level = (OtpErlangAtom) resultTuple.elementAt(0);
    final OtpErlangTuple numbers = (OtpErlangTuple) resultTuple.elementAt(1);
    final OtpErlangList output = (OtpErlangList) resultTuple.elementAt(2);
    return new TestResult() {
      @Override
      public int passed() {
        return ErlUtils.toInt(numbers.elementAt(0));
      }

      @Override
      public int failed() {
        return ErlUtils.toInt(numbers.elementAt(1));
      }

      @Override
      public int skipped() {
        return ErlUtils.toInt(numbers.elementAt(2));
      }

      @Override
      public int cancelled() {
        return ErlUtils.toInt(numbers.elementAt(3));
      }

      @Override
      public void logOutput(Log log) {
        LogLevel logLevel = LogLevel.fromString(level.atomValue());
        for (int i = 0; i < output.arity(); ++i) {
          MavenUtils.logMultiLineString(log, logLevel, ErlUtils.toString(output.elementAt(i), false));
        }
      }
    };
  }
}
