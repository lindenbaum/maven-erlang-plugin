package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;

/**
 * <p>
 * This {@link Mojo} copies the private resources to the
 * {@link AbstractErlangMojo#targetPriv} directory. Default source folders for
 * private resources are {@link AbstractErlangMojo#srcMainPriv} and
 * {@link AbstractErlangMojo#srcMainResourcesPriv}.
 * </p>
 * 
 * @goal copy-resources
 * @phase process-resources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class CopyResourcesMojo extends AbstractErlangMojo {
  @Override
  public void execute() throws MojoExecutionException {
    int copiedFiles = 0;
    copiedFiles += copyDirectory(this.srcMainPriv, this.targetPriv, NULL_FILTER);
    copiedFiles += copyDirectory(this.srcMainResourcesPriv, this.targetPriv, NULL_FILTER);
    if (copiedFiles == 0) {
      this.targetPriv.delete();
    }
    getLog().info("Copied " + copiedFiles + " files");
  }
}
