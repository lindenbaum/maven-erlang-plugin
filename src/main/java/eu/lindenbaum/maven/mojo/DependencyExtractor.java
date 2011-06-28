package eu.lindenbaum.maven.mojo;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.archiver.TarGzUnarchiver;
import eu.lindenbaum.maven.util.CollectionUtils;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.Predicate;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;

/**
 * Unpack {@code erlang-otp} or {@code erlang-std} dependencies. This will
 * unpack all dependencies of this {@link MavenProject} into the
 * {@code target/lib} directory. This is done only in case the dependency has
 * changed since the last unpack process.
 * 
 * @goal extract-dependencies
 * @phase generate-sources
 * @requiresDependencyResolution test
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class DependencyExtractor extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    File lib = p.targetLayout().lib();
    FileUtils.ensureDirectories(lib);
    TarGzUnarchiver unarchiver = new TarGzUnarchiver(p.node(), p.cookie(), lib);
    Collection<Artifact> artifacts = MavenUtils.getErlangArtifacts(p.project());
    if (p.packagingType() == PackagingType.ERLANG_REL) {
      artifacts = filterTestScopeDependencies(artifacts);
    }
    if (artifacts.size() > 0) {
      log.info("Processed project dependencies:");
      for (Artifact artifact : artifacts) {
        extractArtifact(log, artifact, unarchiver);
      }
    }
    cleanupArtifacts(log, lib, artifacts);
  }

  /**
   * Extract a specific artifact (.zip file) into a specific directory.
   */
  private static void extractArtifact(Log log, Artifact artifact, TarGzUnarchiver unarchiver) throws MojoExecutionException {
    File artifactFile = artifact.getFile();
    String artifactdirectory = MavenUtils.getArtifactDirectory(artifact);
    File cachedDependency = new File(unarchiver.getDestination(), artifactdirectory);
    if (!cachedDependency.isDirectory() || artifactFile.lastModified() > cachedDependency.lastModified()) {
      if (cachedDependency.isDirectory()) {
        FileUtils.removeDirectory(cachedDependency);
      }
      try {
        log.info(" * " + artifactdirectory + " (extracted)");
        unarchiver.extract(artifact.getFile());
      }
      catch (IOException e) {
        throw new MojoExecutionException(e.getMessage(), e);
      }
    }
    else {
      log.info(" * " + artifact.getArtifactId() + "-" + artifact.getVersion() + " (skipped)");
    }
  }

  /**
   * Removes obsolete dependencies from previous build runs.
   */
  private static void cleanupArtifacts(Log log, File targetLib, Collection<Artifact> artifacts) {
    List<String> excludes = new ArrayList<String>();
    for (Artifact artifact : artifacts) {
      excludes.add(MavenUtils.getArtifactDirectory(artifact));
    }
    List<File> obsoleteDependencies = FileUtils.getDirectories(targetLib, excludes);
    if (obsoleteDependencies.size() > 0) {
      log.debug("Removed obsolete dependencies:");
      for (File obsoleteDependency : obsoleteDependencies) {
        log.debug(" * " + obsoleteDependency.getName());
        FileUtils.removeDirectory(obsoleteDependency);
      }
    }
  }

  /**
   * Removes {@link Artifact} with scope test from the given collection of
   * artifacts. The input collection is not modified.
   */
  private static Collection<Artifact> filterTestScopeDependencies(Collection<Artifact> artifacts) {
    return CollectionUtils.filter(new Predicate<Artifact>() {
      @Override
      public boolean pred(Artifact artifact) {
        return !Artifact.SCOPE_TEST.equals(artifact.getScope());
      }
    }, artifacts);
  }
}
