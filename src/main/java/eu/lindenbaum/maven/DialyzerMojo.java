package eu.lindenbaum.maven;

import java.io.File;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * Run dialyzer on the generated beam files.
 * 
 * @goal dialyzer
 * @phase process-classes
 */
public final class DialyzerMojo extends AbstractDialyzerMojo {
  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/ebin/"
   */
  private File beamDirectory;

  /**
   * If we should run dialyzer on the generated binaries.
   * 
   * @parameter default-value=false
   */
  private boolean useDialyzer;

  public void execute() throws MojoExecutionException {
    if (this.useDialyzer) {
      if (this.skipDialyzer || this.maven_test_skip) {
        getLog().info("Dialyzer is skipped.");
      }
      else {
        dialyzer(this.beamDirectory);
      }
    }
  }
}
