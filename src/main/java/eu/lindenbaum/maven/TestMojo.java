package eu.lindenbaum.maven;

import java.io.File;
import java.io.FilenameFilter;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.ProcessListener;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * Run the test cases.
 * 
 * @goal test
 * @phase test
 * @requiresDependencyResolution test
 * @author Olivier Sambourg
 * @author Tobias Schlager tobias.schlager@lindenbaum.eu
 */
public final class TestMojo extends AbstractErlMojo implements FilenameFilter {
  /**
   * Regex to distinguish an eunit failure from a success.
   */
  static final Pattern SUCCESS_REGEX = Pattern.compile(".*(?:All\\s+([0-9]+)\\s+tests|Test)\\s+(successful|passed).*");

  /**
   * Set this to 'true' to skip running tests.
   * 
   * @parameter expression="${skipTests}"
   */
  private boolean skipTests;

  /**
   * Set this to 'true' to skip running tests. This also skips dialyzer.
   * 
   * @parameter expression="${maven.test.skip}"
   */
  private boolean maven_test_skip;

  /**
   * Set this to the name of a single test case to run. Defaults to run all test cases.
   * 
   * @parameter expression="${test}"
   */
  private String test;

  /**
   * Set this to true to ignore a failure during testing.
   * 
   * @parameter expression="${maven.test.failure.ignore}"
   */
  private boolean testFailureIgnore;

  /**
   * Set this to "true" to cause a failure if there are no tests to run. Defaults to false.
   * 
   * @parameter expression="${failIfNoTests}"
   */
  private boolean failIfNoTests;

  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/test"
   */
  private File testBeamDirectory;

  /**
   * Source directory.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   */
  private File inputDirectory;

  /**
   * Directory where the reports should be created.
   * 
   * @parameter expression="${project.build.directory}/surefire-reports"
   */
  private File surefireReportsDirectory;

  /**
   * Test files suffix.
   * 
   * @parameter expression="_test"
   */
  private String testSuffix;

  /**
   * If we should use code coverage.
   * 
   * @parameter
   */
  private boolean codeCoverage;

  /**
   * If we should generate surefire-compatible reports.
   * 
   * @parameter
   */
  private boolean surefireReports;

  @Override
  public boolean accept(File dir, String name) {
    String testSuffix = this.testSuffix + ErlConstants.BEAM_SUFFIX;
    if (this.test == null) {
      return name != null && name.endsWith(testSuffix);
    }
    else {
      return name != null && (name.equals(this.test) || name.equals(this.test + testSuffix));
    }
  }

  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    log.info("------------------------------------------------------------------------");
    log.info(" T E S T S");
    log.info("------------------------------------------------------------------------");

    if (this.skipTests || this.maven_test_skip) {
      log.info("Tests are skipped.");
      return;
    }

    String[] testFiles = this.testBeamDirectory.list(this);
    if (this.codeCoverage) {
      File theCoverageDataFile = new File(this.testBeamDirectory, ErlConstants.COVERDATA_BIN);
      if (theCoverageDataFile.exists()) {
        theCoverageDataFile.delete();
      }
    }

    final List<String> commandLine = new LinkedList<String>();
    commandLine.add(getErlCommand(ErlConstants.ERL));
    for (String libPath : getLibPaths()) {
      commandLine.add("-pa");
      commandLine.add(libPath);
    }
    if (this.codeCoverage) {
      commandLine.add("-eval");
      commandLine.add("cover:compile_directory(\"" + this.inputDirectory.getAbsolutePath()
                      + "\", [export_all]).");
      commandLine.add("-run");
      commandLine.add("cover");
      commandLine.add("import");
      commandLine.add(ErlConstants.COVERDATA_BIN);
    }
    final int placeholderIndex;
    if (this.surefireReports) {
      commandLine.add("-eval");
      placeholderIndex = commandLine.size();
      commandLine.add(null); // command line.
    }
    else {
      commandLine.add("-run");
      placeholderIndex = commandLine.size();
      commandLine.add(null); // test file.
      commandLine.add("test");
    }
    if (this.codeCoverage) {
      commandLine.add("-run");
      commandLine.add("cover");
      commandLine.add("export");
      commandLine.add(ErlConstants.COVERDATA_BIN);
    }
    commandLine.add("-noshell");
    commandLine.add("-s");
    commandLine.add("init");
    commandLine.add("stop");

    if (this.surefireReports) {
      if (!this.surefireReportsDirectory.exists()) {
        this.surefireReportsDirectory.mkdir();
      }
    }

    if (testFiles == null || testFiles.length == 0) {
      final String noTestMessage;
      if (this.test == null) {
        noTestMessage = "No tests to run";
      }
      else {
        noTestMessage = "Test case " + this.test + " not found";
      }
      if (this.failIfNoTests) {
        throw new MojoFailureException(noTestMessage + "!");
      }
      else {
        log.info(noTestMessage + ".");
      }
    }
    else {
      final List<String> failedTests = new LinkedList<String>();
      String[] command = commandLine.toArray(new String[0]);
      for (String testFile : testFiles) {
        final String module = testFile.replace(ErlConstants.BEAM_SUFFIX, "");
        log.info("Test case: " + module);
        if (this.surefireReports) {
          command[placeholderIndex] = "eunit:test(" + module
                                      + ", [{order, inorder}, {report,{eunit_surefire,[{dir,\""
                                      + this.surefireReportsDirectory.getPath() + "\"}]}}]).";
        }
        else {
          command[placeholderIndex] = module;
        }
        ErlConstants.exec(command, log, this.testBeamDirectory, new ProcessListener() {
          @Override
          public String processCompleted(int exitValue, List<String> processOutput) throws MojoExecutionException {
            if (exitValue != 0) {
              throw new MojoExecutionException("Error running test cases: " + exitValue);
            }

            boolean testPassed = false;
            for (String line : processOutput) {
              testPassed |= SUCCESS_REGEX.matcher(line).find();
            }
            if (!testPassed) {
              failedTests.add(module);
            }
            return null;
          }
        });
      }
      log.info("------------------------------------------------------------------------");
      if (!failedTests.isEmpty()) {
        log.error("The following tests failed:");
        for (String failedTest : failedTests) {
          log.error(failedTest);
        }
        if (!this.testFailureIgnore) {
          throw new MojoFailureException("There are test failures.");
        }
      }
    }
  }
}
