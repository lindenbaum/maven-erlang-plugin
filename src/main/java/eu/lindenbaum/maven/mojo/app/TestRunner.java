package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
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
  private static final String EUNIT_TESTS_SUFFIX = "_tests" + ErlConstants.BEAM_SUFFIX;

  /**
   * Setting this to {@code true} will skip the test compilation.
   * 
   * @parameter expression="${skipTests}" default-value=false
   */
  private boolean skipTests;

  /**
   * Setting this to a module name, will only run this test case.
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

    File testEbin = p.targetLayout().testEbin();

    List<File> tests = new ArrayList<File>();
    if (this.test == null || this.test.isEmpty()) {
      List<File> candidates = FileUtils.getFilesRecursive(testEbin, ErlConstants.BEAM_SUFFIX);
      tests.addAll(filterTests(p, candidates));
    }
    else {
      File test = new File(testEbin, this.test + ErlConstants.BEAM_SUFFIX);
      if (test.isFile()) {
        tests.add(test);
      }
    }

    if (tests.isEmpty()) {
      log.info("No tests to run.");
      return;
    }

    File surefireReports = p.targetLayout().surefireReports();
    FileUtils.ensureDirectories(surefireReports);

    log.debug("Executing tests:");
    MavenUtils.logCollection(log, LogLevel.DEBUG, tests, " * ");

    String suiteName = p.project().getArtifactId();
    Script<TestResult> script = new TestScript(tests, surefireReports, suiteName);
    TestResult result = MavenSelf.get(p.cookie()).exec(p.testNode(), script);
    result.logOutput(log);

    int passed = result.passed();
    int failed = result.failed();
    int skipped = result.skipped();
    int cancelled = result.cancelled();

    final String summary;
    if ((passed | failed | skipped | cancelled) == 0) {
      summary = "  There were no tests to run.";
    }
    else if ((failed | skipped | cancelled) == 0 && passed == 1) {
      summary = "  Test passed.";
    }
    else if ((failed | skipped | cancelled) == 0 && passed > 1) {
      summary = "  All " + passed + " tests passed.";
    }
    else {
      summary = String.format("  Failed: %s.  Skipped: %s.  Passed: %s.  Cancelled: %s.",
                              failed,
                              skipped,
                              passed,
                              cancelled);
    }

    if (failed + skipped + cancelled > 0) {
      MavenUtils.logCollection(log, LogLevel.ERROR, Arrays.asList(MavenUtils.FAT_SEPARATOR, summary), "");
      throw new MojoFailureException("There were test failures.");
    }
    else {
      MavenUtils.logCollection(log, LogLevel.INFO, Arrays.asList(MavenUtils.FAT_SEPARATOR, summary), "");
    }
  }

  private static List<File> filterTests(Properties p, List<File> tests) {
    List<File> filtered = new ArrayList<File>(tests);
    filtered.removeAll(p.testSupportArtifacts());

    Iterator<File> iterator = filtered.iterator();
    while (iterator.hasNext()) {
      String testPath = iterator.next().getAbsolutePath();
      if (testPath.endsWith(EUNIT_TESTS_SUFFIX)) {
        String impl = testPath.replace(EUNIT_TESTS_SUFFIX, ErlConstants.BEAM_SUFFIX);
        if (tests.contains(new File(impl))) {
          iterator.remove();
        }
      }
    }
    return filtered;
  }
}
