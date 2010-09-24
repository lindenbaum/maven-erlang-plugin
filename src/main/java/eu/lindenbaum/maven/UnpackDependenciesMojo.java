package eu.lindenbaum.maven;

import java.io.File;
import java.util.Enumeration;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.MavenPlexusLogger;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.archiver.zip.ZipUnArchiver;

/**
 * Unpack {@code erlang-otp} dependencies. This will unpack all dependencies of
 * this {@link MavenProject} into the {@link AbstractErlangMojo#targetLib}
 * directory. This is done only in case the dependency has changed since the
 * last unpack process.
 * 
 * @goal unpack-dependencies
 * @phase generate-sources
 * @requiresDependencyResolution test
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class UnpackDependenciesMojo extends AbstractErlangMojo {
  /**
   * Used to extract {@code erlang-otp} archives.
   * 
   * @component role="org.codehaus.plexus.archiver.UnArchiver" roleHint="zip"
   * @required
   */
  private ZipUnArchiver unArchiver;

  @Override
  @SuppressWarnings("unchecked")
  public void execute() throws MojoExecutionException {
    Log log = getLog();
    log.info("Creating dependency directory " + this.targetLib);
    this.targetLib.mkdirs();
    this.unArchiver.setDestDirectory(this.targetLib);
    this.unArchiver.setOverwrite(true);
    this.unArchiver.enableLogging(new MavenPlexusLogger(log));

    Set<Artifact> artifacts = this.project.getArtifacts();
    log.info("Found artifacts " + artifacts);
    for (Artifact artifact : artifacts) {
      if (artifact.getType().equals(ErlConstants.ARTIFACT_TYPE_OTP)) {
        extractArtifact(artifact, this.targetLib);
      }
    }
  }

  /**
   * Extract a specific artifact (.zip file) into a specific directory. 
   * 
   * @param artifact to extract
   * @param dest directory to extract the artifact into
   * @throws MojoExecutionException
   */
  private void extractArtifact(Artifact artifact, File dest) throws MojoExecutionException {
    Log log = getLog();
    try {
      Enumeration<? extends ZipEntry> entries = new ZipFile(artifact.getFile()).entries();
      if (entries.hasMoreElements()) {
        ZipEntry firstElement = entries.nextElement();
        File cachedElement = new File(dest, firstElement.getName());
        if (!cachedElement.exists() || firstElement.getTime() != cachedElement.lastModified()) {
          log.info("Extracting artifact " + artifact.getGroupId() + ":" + artifact.getId());
          this.unArchiver.setSourceFile(artifact.getFile());
          this.unArchiver.extract();
        }
        else {
          log.debug("Skipping artifact " + artifact.getGroupId() + ":" + artifact.getId());
        }
      }
      else {
        log.warn("Artifact " + artifact.getGroupId() + ":" + artifact.getId() + " is empty");
      }
    }
    catch (Exception e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
  }
}
