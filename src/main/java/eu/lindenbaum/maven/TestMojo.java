package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.getLibPaths;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.ProcessListener;

import org.apache.maven.plugin.AbstractMojo;
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
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestMojo extends AbstractMojo implements FilenameFilter {
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
   * Set this to the name of a single test case to run. Defaults to run all test cases.
   * 
   * @parameter expression="${test}"
   */
  private String test;

  /**
   * Set this to "true" to cause a failure if there are no tests to run. Defaults to false.
   * 
   * @parameter expression="${failIfNoTests}"
   */
  private boolean failIfNoTests;

  /**
   * Directories where dependencies are unpacked. This directory contains OTP applications (name-version
   * directories, with include and ebin sub directories).
   * 
   * @parameter expression="${project.build.directory}/lib/"
   */
  private File libDirectory;

  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/test"
   * @required
   */
  private File testBeamDirectory;

  /**
   * Source directory.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   * @required
   */
  private File inputDirectory;

  /**
   * Directory where the reports should be created.
   * 
   * @parameter expression="${project.build.directory}/surefire-reports"
   * @required
   */
  private File reportDirectory;

  @Override
  public boolean accept(File dir, String name) {
    if (this.test == null) {
      return name != null && name.endsWith(ErlConstants.TEST_SUFFIX);
    }
    else {
      return name != null && (name.equals(this.test) || name.equals(this.test + ErlConstants.TEST_SUFFIX));
    }
  }

  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    log.info("------------------------------------------------------------------------");
    log.info(" T E S T S");
    log.info("------------------------------------------------------------------------");

    if (this.skipTests) {
      log.info("Tests are skipped.");
      return;
    }

    String[] testFiles = this.testBeamDirectory.list(this);
    File coverageDataFile = new File(this.testBeamDirectory, ErlConstants.COVERDATA_BIN);
    if (coverageDataFile.exists()) {
      coverageDataFile.delete();
    }

    if (!this.reportDirectory.exists()) {
      this.reportDirectory.mkdir();
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
      for (String testFile : testFiles) {
        final String module = testFile.replace(ErlConstants.BEAM_SUFFIX, "");
        log.info("Test case: " + module);
        ErlUtils.exec(getCommandLine(module), log, this.testBeamDirectory, new ProcessListener() {
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
        throw new MojoFailureException("There are test failures.");
      }
    }
  }

  private List<String> getCommandLine(String testModule) {
    String eunitExpr = "eunit:test(" + testModule + ", [{order, inorder}, {report,{eunit_surefire,[{dir,\""
                       + this.reportDirectory.getPath() + "\"}]}}]).";
    String coverExpr = "cover:compile_directory(\"" + this.inputDirectory.getAbsolutePath()
                       + "\", [export_all]).";

    List<String> command = new ArrayList<String>();
    command.add(ErlConstants.ERL);
    for (File lib : getLibPaths(this.libDirectory)) {
      command.add("-pa");
      command.add(lib.getAbsolutePath());
    }
    command.add("-eval");
    command.add(coverExpr);
    command.add("-run");
    command.add("cover");
    command.add("import");
    command.add(ErlConstants.COVERDATA_BIN);
    command.add("-eval");
    command.add(eunitExpr);
    command.add("-run");
    command.add("cover");
    command.add("export");
    command.add(ErlConstants.COVERDATA_BIN);
    command.add("-noshell");
    command.add("-s");
    command.add("init");
    command.add("stop");
    return Collections.unmodifiableList(command);
  }
}
