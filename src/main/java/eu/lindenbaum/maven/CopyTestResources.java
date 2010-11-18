package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import java.io.File;
import java.io.FileFilter;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;

/**
 * <p>
 * This {@link Mojo} copies test resources to the
 * {@link AbstractErlangMojo#targetTest} directory. Default source folders for
 * test resources are {@link AbstractErlangMojo#srcTestResources}.
 * </p>
 * 
 * @goal copy-test-resources
 * @phase process-test-resources
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class CopyTestResources extends AbstractErlangMojo {
  @Override
  public void execute() throws MojoExecutionException {
    this.targetTest.mkdirs();
    int copiedFiles = 0;
    // TODO: Decide where to copy items from src/test/priv and src/test/resources/priv for the test-phase.
    final File excluded = this.srcTestResourcesPriv;
    copiedFiles += copyDirectory(this.srcTestResources, this.targetTest, new FileFilter() {
      @Override
      public boolean accept(File file) {
        return !file.equals(excluded);
      }
    });
    getLog().info("Copied " + copiedFiles + " file" + (copiedFiles > 1 ? "s" : ""));
  }
}
