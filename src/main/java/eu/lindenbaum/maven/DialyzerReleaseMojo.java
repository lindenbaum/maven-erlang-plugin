package eu.lindenbaum.maven;

import java.io.File;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Run dialyzer on the generated beam files.
 * 
 * @goal dialyzer-release
 * @phase process-classes
 * @requiresDependencyResolution compile
 */
public final class DialyzerReleaseMojo extends AbstractDialyzerMojo {
  /**
   * Directory where the beam files reside.
   * 
   * @parameter expression="${project.build.directory}/ebin/"
   * @required
   */
  private File directory;

  /**
   * Setting this to {@code false} will disable the {@code dialyzer} analysis.
   * 
   * @parameter default-value=false
   */
  private boolean useDialyzer;

  /**
   * Setting this to {@code true} will include the projects dependencies into the {@code dialyzer} run. Note:
   * This may take very long.
   * 
   * @parameter default-value=true
   */
  private boolean dialyzerWithDependencies;

  @Override
  public void execute() throws MojoExecutionException {
    Log log = getLog();
    if (this.useDialyzer) {
      execute(this.directory, this.dialyzerWithDependencies);
    }
    else {
      log.info("Dialyzer is not activated for releases.");
    }
  }
}
