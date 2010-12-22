package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.LoadModulesScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.PurgeModulesScript;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.TestResult;
import eu.lindenbaum.maven.erlang.TestScript;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

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
   * Setting this to {@code true will} will skip the test compilation.
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

  /**
   * Setting this to {@code true} will break the build if there are no tests to
   * run.
   * 
   * @parameter expression="${failIfNoTests}" default-value=false
   */
  private boolean failIfNoTests;

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
      tests.addAll(FileUtils.getFilesRecursive(p.targetTestEbin(), "_test" + ErlConstants.BEAM_SUFFIX));
      tests.addAll(FileUtils.getFilesRecursive(p.targetTestEbin(), "_tests" + ErlConstants.BEAM_SUFFIX));
    }
    else {
      File test = new File(p.targetTestEbin(), this.test + ErlConstants.BEAM_SUFFIX);
      if (test.isFile()) {
        tests.add(test);
      }
    }

    if (tests.isEmpty()) {
      if (this.failIfNoTests) {
        throw new MojoFailureException("No tests to run.");
      }
      else {
        log.info("No tests to run.");
        return;
      }
    }
    p.targetSurefireReports().mkdirs();

    Script<Void> purgeScript = new PurgeModulesScript();
    MavenSelf.get(p.cookie()).exec(p.node(), purgeScript);

    List<File> testCodePaths = new ArrayList<File>();
    testCodePaths.add(p.targetTestEbin());
    testCodePaths.addAll(FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX));

    String suiteName = p.project().getArtifactId();
    Script<TestResult> script = new TestScript(tests, p.targetSurefireReports(), suiteName);
    TestResult result = MavenSelf.get(p.cookie()).eval(p.node(), script, testCodePaths);
    result.logOutput(log);

    List<File> codePaths = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    codePaths.add(p.targetEbin());
    List<File> modules = FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    modules.addAll(FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.BEAM_SUFFIX));
    Script<Integer> loadScript = new LoadModulesScript(modules, codePaths);
    Integer loaded = MavenSelf.get(p.cookie()).exec(p.node(), loadScript);
    log.debug("Successfully reloaded " + loaded + " .beam file(s).");

    if (!result.testsPassed()) {
      throw new MojoFailureException("There were test failures.");
    }
  }
}
