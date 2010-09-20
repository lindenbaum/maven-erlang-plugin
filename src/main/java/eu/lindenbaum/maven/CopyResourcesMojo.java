package eu.lindenbaum.maven;

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
   * @parameter expression="${project.build.directory}/priv"
   */
  private File privDirectory;

  /**
   * Resources/priv directory.
   * @parameter expression="${basedir}/src/main/resources/priv"
   */
  private File resourcesPrivDirectory;

  public void execute() throws MojoExecutionException, MojoFailureException {
    try {
      if (this.resourcesPrivDirectory.exists()) {
        if (!this.privDirectory.exists()) {
          this.privDirectory.mkdirs();
        }
        copyDirectoryStructure(this.resourcesPrivDirectory, this.privDirectory);
      }
    }
    catch (IOException e) {
      throw new MojoExecutionException("Could not copy the resource files", e);
    }
  }
}
