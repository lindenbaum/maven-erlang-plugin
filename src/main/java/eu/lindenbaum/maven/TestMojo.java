package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.COVERDATA_BIN;
import static eu.lindenbaum.maven.util.ErlConstants.ERL;
import static eu.lindenbaum.maven.util.ErlConstants.ERL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.TEST_SUFFIX;
import static eu.lindenbaum.maven.util.FileUtils.extractFilesFromJar;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;
import static eu.lindenbaum.maven.util.FileUtils.getDependencyIncludes;
import static eu.lindenbaum.maven.util.FileUtils.getFilesRecursive;
import static eu.lindenbaum.maven.util.MavenUtils.getPluginFile;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.Observer;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * {@link Mojo} running test cases.
 * </p>
 * <p>
 * This {@link Mojo} will either run a specific test case (if the user specified
 * one using {@code -Dtest=test_module_test}) or run all test cases found in the
 * {@link AbstractErlangMojo#srcTestErlang} directory.
 * </p>
 * <p>
 * The user can also choose to skip testing by specifying {@code -DskipTests}. A
 * test code coverage will automatically be applied. A report about the test
 * coverage may be created using the {@link CoverageReport}.
 * </p>
 * 
 * @goal test
 * @phase test
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @see CoverageReport
 */
public final class TestMojo extends AbstractErlangMojo {
  static final Pattern SUCCESS_REGEX = Pattern.compile(".*(?:All\\s+([0-9]+)\\s+tests|Test)\\s+(successful|passed).*");

  /**
   * Setting this to a module name, will only run this test case.
   * 
   * @parameter
   */
  private String test;

  /**
   * Setting this to {@code true will} skip the test runs.
   * 
   * @parameter default-value=false
   */
  private boolean skipTests;

  /**
   * Setting this to {@code true} will break the build if there are no tests to
   * run.
   * 
   * @parameter default-value=false
   */
  private boolean failIfNoTests;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    final Log log = getLog();
    log.info("------------------------------------------------------------------------");
    log.info(" T E S T S");
    log.info("------------------------------------------------------------------------");

    if (this.skipTests) {
      log.info("Tests are skipped.");
    }
    else {
      this.targetSurefireReports.mkdirs();
      File coverageDataFile = new File(this.targetTest, COVERDATA_BIN);
      coverageDataFile.delete();

      List<String> modules = getTestCases(this.targetTest, this.test);
      if (modules.size() > 0) {
        log.info("Test cases: " + modules.toString());

        File plugin = getPluginFile("maven-erlang-plugin", this.project, this.repository);
        extractFilesFromJar(plugin, ERL_SUFFIX, this.targetTest);

        List<String> command = getCommandLine(modules);
        ErlUtils.exec(command, log, this.targetTest, new Observer() {
          @Override
          public String handle(int exitValue, String result) throws MojoExecutionException,
                                                            MojoFailureException {
            if (exitValue != 0) {
              throw new MojoExecutionException("Unit test run returned with " + exitValue);
            }
            if (!SUCCESS_REGEX.matcher(result).find()) {
              log.info("------------------------------------------------------------------------");
              log.error("Tests failed.");
              throw new MojoFailureException("There are test failures.");
            }
            return null;
          }
        });
      }
      else {
        log.info("No tests to run.");
        if (this.failIfNoTests) {
          throw new MojoFailureException("build failure due to missing tests");
        }
      }
    }
  }

  /**
   * Returns a {@link List} of test modules to run. In case the single test case
   * parameter {@link #test} is specified this function tries to determine
   * whether this test can be run. Otherwise all runnable tests are returned.
   * 
   * @param directory where the compiled tests reside
   * @param singleTest name of a specific single test to run
   * @return a {@link List} of test modules to run
   */
  private static List<String> getTestCases(File directory, String singleTest) {
    final List<File> tests;
    if (singleTest == null) {
      tests = getFilesRecursive(directory, TEST_SUFFIX);
    }
    else {
      File test = new File(directory, singleTest + TEST_SUFFIX);
      if (test.exists()) {
        tests = Arrays.asList(new File[]{ test });
      }
      else {
        test = new File(directory, singleTest);
        if (test.exists()) {
          tests = Arrays.asList(new File[]{ test });
        }
        else {
          tests = getFilesRecursive(directory, TEST_SUFFIX);
        }
      }
    }
    List<String> modules = new ArrayList<String>();
    for (File test : tests) {
      modules.add(test.getName().replace(BEAM_SUFFIX, ""));
    }
    return Collections.unmodifiableList(modules);
  }

  /**
   * Returns the command line executing all unit tests.
   * 
   * @param modules a list of test cases to run
   * @return an executable command list
   */
  private List<String> getCommandLine(List<String> modules) {
    StringBuilder coverCompileStr = new StringBuilder("cover2:compile_directory(\"");
    coverCompileStr.append(this.srcMainErlang.getAbsolutePath());
    coverCompileStr.append("\", [export_all");
    List<File> includes = new ArrayList<File>();
    includes.addAll(Arrays.asList(new File[]{ this.srcMainInclude, this.srcTestInclude, this.targetInclude }));
    includes.addAll(getDependencyIncludes(this.targetLib));
    for (File inc : includes) {
      if (inc != null && inc.isDirectory()) {
        coverCompileStr.append(", {i, \"" + inc.getAbsolutePath() + "\"}");
      }
    }
    coverCompileStr.append("]).");
    String coverExpr = coverCompileStr.toString();

    String eunitExpr = "eunit:test(" //
                       + modules.toString() //
                       + ", [{report,{surefire,[{dir,\"" //
                       + this.targetSurefireReports.getAbsolutePath()//
                       + "\"}, {package, \""//
                       + this.project.getArtifactId() //
                       + ".\"}]}}]).";

    List<String> command = new ArrayList<String>();
    command.add(ERL);
    for (File lib : getDependencies(this.targetLib)) {
      command.add("-pa");
      command.add(lib.getAbsolutePath());
    }
    command.add("-eval");
    command.add("c:c(cover2),c:c(surefire),c:c(mock).");
    command.add("-eval");
    command.add(coverExpr);
    command.add("-eval");
    command.add(eunitExpr);
    command.add("-run");
    command.add("cover2");
    command.add("export");
    command.add(COVERDATA_BIN);
    command.add("-noshell");
    command.add("-s");
    command.add("init");
    command.add("stop");
    return Collections.unmodifiableList(command);
  }
}
