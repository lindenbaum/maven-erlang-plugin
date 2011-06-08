package eu.lindenbaum.maven.erlang;

import java.io.File;
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
 * A script that returns the coverage report for some project.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class CoverageReportScript extends AbstractScript<CoverageReportResult> {
  private final List<File> tests;
  private final File testDir;
  private final List<File> sources;
  private final File coverageReportDir;
  private final String coverageReportName;

  public CoverageReportScript(File testDir,
                              List<File> tests,
                              List<File> sources,
                              File coverageReportDir,
                              String coverageReportName) throws MojoExecutionException {
    super();
    this.testDir = testDir;
    this.tests = tests;
    this.sources = sources;
    this.coverageReportDir = coverageReportDir;
    this.coverageReportName = coverageReportName;
  }

  @Override
  public String get() {
    String testPath = this.testDir.getAbsolutePath();
    String testModuleList = ErlUtils.toModuleList(this.tests, "'", "'");
    String sourceModuleList = ErlUtils.toModuleList(this.sources, "'", "'");
    return String.format(this.script,
                         testPath,
                         testModuleList,
                         sourceModuleList,
                         this.coverageReportDir,
                         this.coverageReportName);
  }

  @Override
  public CoverageReportResult handle(OtpErlangObject output) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) output;
    final String result = ErlUtils.toString(resultTuple.elementAt(0));
    final OtpErlangList resultList = (OtpErlangList) resultTuple.elementAt(1);
    return new CoverageReportResult() {

      @Override
      public boolean failed() {
        return "error".equals(result);
      }

      @Override
      public void logOutput(Log log) {
        if (failed()) {
          for (int i = 0; i < resultList.arity(); ++i) {
            String message = ErlUtils.toString(resultList.elementAt(i));
            MavenUtils.logMultiLineString(log, LogLevel.ERROR, message);
          }
        }
      }
    };
  }
}
