package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;

/**
 * Copy resources to target/priv.
 * 
 * @goal copy-resources
 * @phase generate-resources
 */
public final class CopyResourcesMojo extends AbstractMojo {
  /**
   * Directory to put the private resources into.
   * 
   * @parameter expression="${project.build.directory}/priv"
   * @required
   */
  private File outputDirectory;

  /**
   * The source main folder of this project. This may be used to include sources from other languages into the
   * erlang application.
   * 
   * @parameter expression="${basedir}/src/main/"
   */
  private File srcDir;

  /**
   * First possible priv directory.
   * 
   * @parameter expression="${basedir}/src/main/resources/priv"
   */
  private File inputDirectory1;

  /**
   * Second possible priv directory.
   * 
   * @parameter expression="${basedir}/src/main/erlang/priv"
   */
  private File inputDirectory2;

  @Override
  public void execute() throws MojoExecutionException {
    try {
      int copiedFiles = 0;
      copiedFiles += copyDirectory(this.srcDir, this.outputDirectory, new FileFilter() {
        @Override
        public boolean accept(File file) {
          return file.isDirectory() && file.getName().equals("erlang");
        }
      });
      copiedFiles += copyDirectory(this.inputDirectory1, this.outputDirectory, NULL_FILTER);
      copiedFiles += copyDirectory(this.inputDirectory2, this.outputDirectory, NULL_FILTER);
    }
    catch (IOException e) {
      throw new MojoExecutionException("failed to copy resources " + e.getMessage(), e);
    }
  }
}
