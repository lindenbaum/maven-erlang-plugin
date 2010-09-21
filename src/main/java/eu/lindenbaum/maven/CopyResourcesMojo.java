package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import java.io.File;
import java.io.IOException;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * Copy resources from resources/priv to target/priv.
 * 
 * @goal copy-resources
 * @phase generate-resources
 */
public final class CopyResourcesMojo extends AbstractErlMojo {
  /**
   * Directory where the private files were created.
   * 
   * @parameter expression="${project.build.directory}/priv"
   */
  private File privDirectory;

  /**
   * Resources/priv directory.
   * 
   * @parameter expression="${basedir}/src/main/resources/priv"
   */
  private File resourcesPrivDirectory;

  public void execute() throws MojoExecutionException, MojoFailureException {
    if (this.resourcesPrivDirectory.exists()) {
      try {
        copyDirectory(this.resourcesPrivDirectory, this.privDirectory, NULL_FILTER);
      }
      catch (IOException e) {
        throw new MojoExecutionException("failed to copy resources " + e.getMessage(), e);
      }
    }
  }
}
