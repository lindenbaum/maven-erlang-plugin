package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.CoverageReportResult;
import eu.lindenbaum.maven.erlang.CoverageReportResult.Report;
import eu.lindenbaum.maven.erlang.CoverageReportResult.Report.Module;
import eu.lindenbaum.maven.erlang.CoverageReportScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.report.CoverageReport;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Runs a test coverage analysis on the modules in of the project, optionally 
 * printing the result to console. The coverage data is saved as a simple text 
 * file (<tt>COVERAGE-${project.artifactId}.txt</tt>) with simple space 
 * separated list of results, with coverage levels for: modules, functions, 
 * clauses and lines. See also {@link CoverageReport}, i.e. the 
 * {@code coverage-report} goal.
 *
 * <p>
 * ISSUE If a test purges or unloads a module to do coverage for, the coverage
 * compilation information will be gone and the coverage report will fail.
 * </p>
 * 
 * @goal coverage
 * @execute phase="test-compile"
 * @phase pre-site lifecycle="site"
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @since 2.1.0
 * @see CoverageReport
 */
public class Coverage extends ErlangMojo {
  /**
   * Setting this to {@code true} will silent the console output and only
   * generate the coverage output file.
   * 
   * @parameter expression="${silent}" default-value="false"
   */
  private boolean silent;

  /**
   * Setting this to {@code true} will print the coverage for each module, and
   * not only the coverage summary.
   * 
   * @parameter expression="${details}" default-value="false"
   */
  private boolean details;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" C O V E R A G E");
    log.info(MavenUtils.SEPARATOR);

    File testEbin = p.targetLayout().testEbin();

    if (!testEbin.exists()) {
      log.info("Nothing to do.");
      return;
    }

    File[] testFiles = testEbin.listFiles(FileUtils.BEAM_FILTER);
    if (testFiles == null || testFiles.length == 0) {
      log.info("Nothing to do.");
      return;
    }

    List<File> tests = new ArrayList<File>();
    tests.addAll(FileUtils.getFilesRecursive(testEbin, "_test" + ErlConstants.BEAM_SUFFIX));
    tests.addAll(FileUtils.getFilesRecursive(testEbin, "_tests" + ErlConstants.BEAM_SUFFIX));

    File src = p.sourceLayout().src();

    List<File> sources = FileUtils.getFilesRecursive(src, ErlConstants.ERL_SUFFIX);

    File coverageReportDir = p.targetLayout().coverageReports();
    FileUtils.ensureDirectories(coverageReportDir);
    String coverageReportName = p.project().getArtifactId();

    Script<CoverageReportResult> script = new CoverageReportScript(testEbin,
                                                                   tests,
                                                                   sources,
                                                                   coverageReportDir,
                                                                   coverageReportName);

    log.info("Running coverage tests...");
    CoverageReportResult result = MavenSelf.get(p.cookie()).exec(p.testNode(), script);

    if (result.failed()) {
      result.logOutput(getLog());
      throw new MojoExecutionException("failed to generate coverage report");
    }

    if (this.silent) {
      log.info("Successfully generated coverage.");
      return;
    }

    List<File> reports = FileUtils.getFilesRecursive(coverageReportDir, ".txt");
    if (reports.isEmpty()) {
      throw new MojoExecutionException("No coverage report files found at: " + coverageReportDir);
    }

    Report report = new CoverageReportResult.Report(reports.get(0));

    if (this.details) {
      printModulesSummary(getLog(), report);
    }
    printReportSummary(getLog(), report);
    log.info("Successfully generated coverage.");
  }

  private void printModulesSummary(Log log, Report report) {
    log.info("MODULES");
    log.info(MavenUtils.FAT_SEPARATOR);
    for (Module module : report.getModules()) {
      String name = module.getName() + ErlConstants.ERL_SUFFIX;
      int padding = 69 - name.length();
      String covered = module.getCoverage() == 100 ? "COVERED" : "NOT COVERED!";
      log.info(String.format("> %1$s %2$" + padding + "s", name, covered));
      log.info(MavenUtils.SEPARATOR);
      log.info(String.format("Coverage: %1$23d%% | Lines: %2$28d",
                             module.getCoverage(),
                             module.getNumberOfLines()));
      log.info(String.format("Functions: %1$23d | Covered lines: %2$20d",
                             module.getNumberOfFunctions(),
                             module.getNumberOfCoveredLines()));
      log.info(String.format("Clauses: %1$25d | Not covered lines: %2$16d",
                             module.getNumberOfClauses(),
                             module.getNumberOfNotCoveredLines()));
      log.info(MavenUtils.SEPARATOR);
    }
    log.info("");
  }

  private void printReportSummary(Log log, Report report) {
    log.info("SUMMARY");
    log.info(MavenUtils.FAT_SEPARATOR);
    log.info(String.format("Total coverage:%1$56d%%", report.getCoverage()));
    log.info(MavenUtils.SEPARATOR);
    log.info(String.format("Modules:%1$26d | Lines:%2$29d",
                           report.getNumberOfModules(),
                           report.getNumberOfLines()));
    log.info(String.format("Functions:%1$24d | Covered lines:%2$21d",
                           report.getNumberOfFunctions(),
                           report.getNumberOfCoveredLines()));
    log.info(String.format("Clauses:%1$26d | Not covered lines:%2$17d",
                           report.getNumberOfClauses(),
                           report.getNumberOfNotCoveredLines()));
    log.info(MavenUtils.SEPARATOR);
  }
}
