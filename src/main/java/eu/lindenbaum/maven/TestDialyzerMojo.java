package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.DialyzerUtils.dialyze;
import static eu.lindenbaum.maven.util.DialyzerUtils.needDialyzerBuild;
import static eu.lindenbaum.maven.util.FileUtils.getLibPaths;

import java.io.File;
import java.util.Collections;
import java.util.List;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Run dialyzer on the generated test beam files.
 * 
 * @goal test-dialyzer
 * @phase process-test-classes
 * @requiresDependencyResolution test-compile
 */
public final class TestDialyzerMojo extends AbstractMojo {
  /**
   * Additional dialyzer options.
   * 
   * @parameter
   */
  private String[] dialyzerOptions;

  /**
   * Directories where dependencies are unpacked. This directory contains OTP applications (name-version
   * directories, with include and ebin sub directories).
   * 
   * @parameter expression="${project.build.directory}/lib/"
   * @required
   */
  private File libDirectory;

  /**
   * If dialyzer warnings are to be considered as errors.
   * 
   * @parameter default-value=false
   */
  private boolean dialyzerWarningsAreErrors;

  /**
   * If dialyzer should be run with the dependencies. This is *much* slower.
   * 
   * @parameter default-value=false
   */
  private boolean dialyzerWithDependencies;

  /**
   * Set this to 'true' to force running dialyzer, i.e. even if beams are not newer than latest run.
   * 
   * @parameter expression="${forceDialyzer}"
   */
  private boolean forceDialyzer;

  /**
   * Set this to 'true' to skip dialyzer.
   * 
   * @parameter expression="${skipDialyzer}"
   */
  protected boolean skipDialyzer;

  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/test"
   * @required
   */
  private File testBeamDirectory;

  /**
   * If we should run dialyzer on the generated binaries.
   * 
   * @parameter default-value=false
   */
  private boolean testUseDialyzer;

  public void execute() throws MojoExecutionException {
    Log log = getLog();
    if (this.testUseDialyzer) {
      if (this.skipDialyzer) {
        log.info("Dialyzer is skipped.");
      }
      else {
        if (this.forceDialyzer || needDialyzerBuild(this.testBeamDirectory)) {
          final List<File> libPaths;
          if (this.dialyzerWithDependencies) {
            libPaths = getLibPaths(this.libDirectory);
          }
          else {
            libPaths = Collections.emptyList();
          }
          dialyze(log, this.testBeamDirectory, libPaths, this.dialyzerOptions, this.dialyzerWarningsAreErrors);
        }
        else {
          log.info("No need for new dialyzer run.");
        }
      }
    }
    else {
      log.info("Dialyzer is not activated for tests.");
    }
  }
}
