package eu.lindenbaum.maven.mojo.app;

import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.removeDirectory;

import java.io.File;
import java.io.IOException;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Copies all test resource files into that target directory structure. Copied
 * resources contain:
 * <ul>
 * <li>resources (*)</li>
 * <li>test resources (*)</li>
 * </ul>
 * 
 * @goal generate-test-resources
 * @phase generate-test-resources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestResourceGenerator extends ErlangMojo {
  /**
   * Setting this to {@code true} will skip copying the test resources.
   * 
   * @parameter expression="${skipTests}" default-value=false
   */
  private boolean skipTests;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    if (this.skipTests) {
      return;
    }

    removeDirectory(p.targetTest());

    int testResources = 0;
    testResources += copyDirectory(p.priv(), p.targetTestPriv(), FileUtils.NULL_FILTER);
    testResources += copyDirectory(p.test_priv(), p.targetTestPriv(), FileUtils.NULL_FILTER);
    log.debug("copied " + testResources + " test resources");
    if (testResources == 0) {
      p.targetTest().delete();
    }

    int foreignArtifacts = 0;
    for (Artifact artifact : MavenUtils.getForeignDependencies(p.project())) {
      File source = artifact.getFile();
      File destination = new File(p.targetTestPriv(), source.getName());
      try {
        org.codehaus.plexus.util.FileUtils.copyFile(source, destination);
        foreignArtifacts++;
      }
      catch (IOException e) {
        log.error("Failed to copy artifact " + source.getPath() + " to " + p.targetPriv() + ".", e);
      }
    }
    log.debug("copied " + foreignArtifacts + " foreign artifacts");
  }
}
