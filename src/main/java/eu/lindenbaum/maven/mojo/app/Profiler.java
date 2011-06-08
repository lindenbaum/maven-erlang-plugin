package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.ProfilingResult;
import eu.lindenbaum.maven.erlang.ProfilingResult.Report;
import eu.lindenbaum.maven.erlang.ProfilingResult.Report.Row;
import eu.lindenbaum.maven.erlang.ProfilingScript;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.report.ProfilingReport;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * Runs the test modules, recording the number of function calls, and the
 * execution time, of modules in the current project using 
 * <a href="http://www.erlang.org/doc/man/eprof.html">eprof</a>. The profiling
 * result are filtered and written to a file
 * (<tt>PROFILING-${project.artifactId}.txt</tt>) that can be used to generate
 * a {@code profiling-report}.
 * 
 * @goal profile
 * @execute phase="test-compile"
 * @phase pre-site lifecycle="site"
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @since 2.1.0
 * @see ProfilingReport
 */
public final class Profiler extends ErlangMojo {
  /**
   * Setting this to a module name, will only run and profile that test module.
   * 
   * @parameter expression="${test}"
   */
  private String test;

  /**
   * The time, in seconds, that the full profiling is allowed to take. Must be a
   * positive integer. If the profiling tests run longer than this, without
   * responding with any results, it is considered to have failed.
   * 
   * @parameter expression="${timeout}" default-value=600
   */
  private int timeout;

  /**
   * Setting this to {@code true} will silent the console output and only
   * generate the profiling output file.
   * 
   * @parameter expression="${silent}" default-value="false"
   */
  private boolean silent;

  /**
   * Setting this to {@code true} will print the complete profiling results, and
   * not only the profiling top-10.
   * 
   * @parameter expression="${details}" default-value="false"
   */
  private boolean details;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" P R O F I L E R");
    log.info(MavenUtils.SEPARATOR);

    if (this.timeout <= 0) {
      throw new IllegalArgumentException("timeout must be a positive integer");
    }

    File testEbin = p.targetLayout().testEbin();

    List<File> tests = new ArrayList<File>();
    if (this.test == null || this.test.isEmpty()) {
      tests.addAll(FileUtils.getFilesRecursive(testEbin, "_test" + ErlConstants.BEAM_SUFFIX));
      tests.addAll(FileUtils.getFilesRecursive(testEbin, "_tests" + ErlConstants.BEAM_SUFFIX));
    }
    else {
      File test = new File(testEbin, this.test + ErlConstants.BEAM_SUFFIX);
      if (test.isFile()) {
        tests.add(test);
      }
    }

    if (tests.isEmpty()) {
      log.info("No tests to profile.");
      return;
    }

    File profilingReportsDir = p.targetLayout().profilingReports();
    String profilingReportName = p.project().getArtifactId();

    FileUtils.ensureDirectories(profilingReportsDir);

    Script<ProfilingResult> script = new ProfilingScript(tests,
                                                         profilingReportsDir,
                                                         profilingReportName,
                                                         this.timeout);
    log.info("Running profiling tests...");
    ProfilingResult result = MavenSelf.get(p.cookie()).exec(p.testNode(), script);

    if (!result.testsPassed()) {
      result.logOutput(log);
      throw new MojoFailureException("There were test failures.");
    }

    if (this.silent) {
      log.info("Successfully generated profiling.");
      return;
    }

    List<File> reports = FileUtils.getFilesRecursive(profilingReportsDir, ".txt");
    if (reports.isEmpty()) {
      throw new MojoExecutionException("No profiling report files found at: " + profilingReportsDir);
    }
    Report report = new ProfilingResult.Report(reports.get(0), p);
    printProfiling(report, log);
    log.info("Successfully generated profiling.");
  }

  private void printProfiling(Report report, Log log) {
    final int maxItems = this.details ? report.getRows().size() : 10;
    Iterator<Row> rows = report.getRows().iterator();
    log.info(MavenUtils.SEPARATOR + "--------------------");
    log.info("FUNCTIONS                                                             CALLS   TIME  uS/CALLS");
    log.info(MavenUtils.FAT_SEPARATOR + "====================");
    for (int i = maxItems; i > 0 && rows.hasNext(); i--) {
      Row row = rows.next();
      log.info(String.format("%1$-68s %2$6d %3$6d %4$9.2f",
                             row.name,
                             row.calls,
                             row.time,
                             row.microSecondsPerCall));
    }
    log.info(MavenUtils.SEPARATOR + "--------------------");
  }
}
