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
 * @requiresDependencyResolution test
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

    Collection<File> current = new ArrayList<File>();
    current.addAll(copyIncludes(p));
    current.addAll(copyResources(p));
    current.addAll(copyNonErlangDependencies(log, p));

    if (current.size() > 0) {
      log.debug("Copied test resources:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
    else {
      FileUtils.removeEmptyDirectory(p.targetLayout().test());
    }
  }

  private static Collection<File> copyIncludes(Properties p) throws MojoExecutionException {
    File include = p.sourceLayout().include();
    File testInclude = p.sourceLayout().testInclude();
    File targetInclude = p.targetLayout().testInclude();
    FileUtils.removeDirectory(targetInclude);

    Collection<File> copied = new ArrayList<File>();
    copied.addAll(FileUtils.copyDirectory(include, targetInclude, FileUtils.SOURCE_FILTER));
    copied.addAll(FileUtils.copyDirectory(testInclude, targetInclude, FileUtils.SOURCE_FILTER));
    return copied;
  }

  private static Collection<File> copyResources(Properties p) throws MojoExecutionException {
    File priv = p.sourceLayout().priv();
    File testPriv = p.sourceLayout().testPriv();
    File targetPriv = p.targetLayout().testPriv();
    FileUtils.removeDirectory(targetPriv);

    Collection<File> copied = new ArrayList<File>();
    copied.addAll(FileUtils.copyDirectory(priv, targetPriv, FileUtils.NULL_FILTER));
    copied.addAll(FileUtils.copyDirectory(testPriv, targetPriv, FileUtils.NULL_FILTER));
    return copied;
  }

  private static Collection<File> copyNonErlangDependencies(Log log, Properties p) {
    File targetPriv = p.targetLayout().testPriv();
    Collection<File> copied = new ArrayList<File>();
    for (Artifact artifact : MavenUtils.getForeignDependencies(p.project())) {
      File source = artifact.getFile();
      File destination = new File(targetPriv, source.getName());
      try {
        org.codehaus.plexus.util.FileUtils.copyFile(source, destination);
        copied.add(source);
      }
      catch (IOException e) {
        log.error("Failed to copy artifact " + source.getPath() + " to " + targetPriv + ".", e);
      }
    }
    return copied;
  }
}
