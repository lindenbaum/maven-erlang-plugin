package eu.lindenbaum.maven;

import java.io.File;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * Run dialyzer on the generated test beam files.
 * 
 * @goal test-dialyzer
 * @phase process-test-classes
 */
public final class TestDialyzerMojo extends AbstractDialyzerMojo {
  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/test"
   */
  private File testBeamDirectory;

  /**
   * If we should run dialyzer on the generated binaries.
   * 
   * @parameter default-value=false
   */
  private boolean testUseDialyzer;

  public void execute() throws MojoExecutionException {
    if (this.testUseDialyzer) {
      if (this.skipDialyzer || this.maven_test_skip) {
        getLog().info("Dialyzer is skipped.");
      }
      else {
        dialyzer(this.testBeamDirectory);
      }
    }
  }
}
