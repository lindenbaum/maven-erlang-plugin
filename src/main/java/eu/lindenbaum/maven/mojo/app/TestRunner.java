package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.TestResult;
import eu.lindenbaum.maven.erlang.TestScript;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;
import eu.lindenbaum.maven.util.MojoUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Mojo} that runs test modules using {@code eunit}.
 * 
 * @goal test
 * @phase test
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @author Olivier Sambourg
 */
public final class TestRunner extends ErlangMojo {
  /**
   * Setting this to {@code true} will skip the test compilation.
   * 
   * @parameter expression="${skipTests}" default-value=false
   */
  private boolean skipTests;

  /**
   * Setting this to some test module name, or a comma separated list of test 
   * module names, will only run those tests.
   * 
   * @parameter expression="${test}"
   */
  private String test;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" T E S T - R U N N E R");
    log.info(MavenUtils.SEPARATOR);

    if (this.skipTests) {
      log.info("Tests are skipped.");
      return;
    }

    List<File> tests = new ArrayList<File>();
    if (this.test == null || this.test.isEmpty()) {
      tests.addAll(MojoUtils.getEunitTestSet(p.modules(true, false), p.testSupportArtifacts()));
    }
    else {
      String[] testNames = this.test.split(",");
      for (String testName : testNames) {
        File test = new File(p.targetLayout().testEbin(), testName.trim() + ErlConstants.BEAM_SUFFIX);
        if (test.isFile()) {
          tests.add(test);
        }
      }
    }

    if (tests.isEmpty()) {
      log.info("No tests to run.");
      return;
    }

    File surefireReports = p.targetLayout().surefireReports();
    FileUtils.ensureDirectories(surefireReports);

    log.debug(String.format("Executing %s test(s):", tests.size()));
    MavenUtils.logCollection(log, LogLevel.DEBUG, tests, " * ");

    String suiteName = p.project().getArtifactId();
    Script<TestResult> script = new TestScript(tests, surefireReports, suiteName);
    TestResult result = MavenSelf.get(p.cookie()).exec(p.testNode(), script);
    result.logOutput(log);

    int passed = result.passed();
    int failed = result.failed();
    int skipped = result.skipped();
    int cancelled = result.cancelled();

    if ((passed | failed | skipped | cancelled) == 0) {
      log.warn("  There were no tests to run.");
    }
    else if ((failed | skipped | cancelled) == 0 && passed == 1) {
      log.info("  Test passed.");
    }
    else if ((failed | skipped | cancelled) == 0 && passed > 1) {
      log.info("  All " + passed + " tests passed.");
    }
    else {
      log.error(MavenUtils.FAT_SEPARATOR);
      log.error(String.format("  Failed: %s.  Skipped: %s.  Passed: %s.  Cancelled: %s.",
                              failed,
                              skipped,
                              passed,
                              cancelled));
      throw new MojoFailureException("There were test failures.");
    }
  }
}
