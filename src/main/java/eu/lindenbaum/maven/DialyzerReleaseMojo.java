package eu.lindenbaum.maven;

import java.io.File;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * Run dialyzer on the generated beam files.
 * 
 * @goal dialyzer-release
 * @phase process-classes
 */
public final class DialyzerReleaseMojo extends AbstractDialyzerMojo {
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

  /**
   * If dialyzer should be run with the dependencies. This is *much* slower.
   * 
   * @parameter default-value=true
   */
  private boolean dialyzerWithDependencies;

  public void execute() throws MojoExecutionException {
    if (this.useDialyzer) {
      if (this.skipDialyzer || this.maven_test_skip) {
        getLog().info("Dialyzer is skipped.");
      }
      else {
        setDialyzerWithDependencies(this.dialyzerWithDependencies);
        dialyzer(this.beamDirectory);
      }
    }
  }
}
