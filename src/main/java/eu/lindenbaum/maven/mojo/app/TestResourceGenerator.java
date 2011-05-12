package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

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

    Collection<File> current;

    FileUtils.removeDirectory(p.targetTestPriv());
    current = FileUtils.copyDirectory(p.priv(), p.targetTestPriv(), FileUtils.NULL_FILTER);
    current.addAll(FileUtils.copyDirectory(p.test_priv(), p.targetTestPriv(), FileUtils.NULL_FILTER));
    if (current.size() > 0) {
      log.debug("Copied test resources:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
    else {
      FileUtils.removeEmptyDirectory(p.targetTest());
    }

    current = new ArrayList<File>();
    for (Artifact artifact : MavenUtils.getForeignDependencies(p.project())) {
      File source = artifact.getFile();
      File destination = new File(p.targetTestPriv(), source.getName());
      try {
        org.codehaus.plexus.util.FileUtils.copyFile(source, destination);
        current.add(source);
      }
      catch (IOException e) {
        log.error("Failed to copy artifact " + source.getPath() + " to " + p.targetPriv() + ".", e);
      }
    }
    if (current.size() > 0) {
      log.debug("Copied foreign artifacts:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
  }
}
