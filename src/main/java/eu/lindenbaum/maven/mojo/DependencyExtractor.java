package eu.lindenbaum.maven.mojo;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.archiver.TarGzUnarchiver;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

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
    File targetLib = p.targetLib();
    FileUtils.ensureDirectories(targetLib);
    TarGzUnarchiver unarchiver = new TarGzUnarchiver(p.node(), p.cookie(), targetLib);
    Set<Artifact> artifacts = MavenUtils.getErlangArtifacts(p.project());
    log.debug("found artifacts " + artifacts);
    for (Artifact artifact : artifacts) {
      extractArtifact(log, artifact, unarchiver);
    }
    cleanupArtifacts(log, targetLib, artifacts);
  }

  /**
   * Extract a specific artifact (.zip file) into a specific directory.
   */
  private static void extractArtifact(Log log, Artifact artifact, TarGzUnarchiver unarchiver) throws MojoExecutionException {
    File artifactFile = artifact.getFile();
    String artifactdirectory = getArtifactDirectory(artifact);
    File cachedDependency = new File(unarchiver.getDestination(), artifactdirectory);
    if (!cachedDependency.isDirectory() || artifactFile.lastModified() > cachedDependency.lastModified()) {
      if (cachedDependency.isDirectory()) {
        FileUtils.removeDirectory(cachedDependency);
      }
      try {
        log.info("Extracting dependency " + artifactdirectory + ".");
        unarchiver.extract(artifact.getFile());
      }
      catch (IOException e) {
        throw new MojoExecutionException(e.getMessage(), e);
      }
    }
    else {
      log.debug("Skipping artifact " + artifact.getGroupId() + ":" + artifact.getId());
    }
  }

  /**
   * Removes obsolete dependencies from previous build runs.
   */
  private static void cleanupArtifacts(Log log, File targetLib, Set<Artifact> artifacts) {
    List<String> excludes = new ArrayList<String>();
    for (Artifact artifact : artifacts) {
      excludes.add(getArtifactDirectory(artifact));
    }
    List<File> obsoleteDependencies = FileUtils.getDirectories(targetLib, excludes);
    for (File obsoleteDependency : obsoleteDependencies) {
      log.debug("Removing obsolete dependency " + obsoleteDependency.getName() + ".");
      FileUtils.removeDirectory(obsoleteDependency);
    }
  }

  /**
   * Returns the directory name for the given {@link Artifact}.
   */
  private static String getArtifactDirectory(Artifact artifact) {
    return artifact.getFile().getName().replace("." + artifact.getType(), "");
  }
}
