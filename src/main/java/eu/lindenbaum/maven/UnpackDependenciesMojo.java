package eu.lindenbaum.maven;

import java.io.File;
import java.util.Enumeration;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.MavenPlexusLogger;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.archiver.zip.ZipUnArchiver;

/**
 * Unpack {@code erlang-otp} dependencies.
 * 
 * @goal unpack-dependencies
 * @phase generate-sources
 */
public final class UnpackDependenciesMojo extends AbstractMojo {
  /**
   * {@link MavenProject} to process.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Directory where dependencies are unpacked.
   * 
   * @parameter expression="${project.build.directory}/lib/"
   * @required
   */
  private File libOutput;

  /**
   * Used to extract {@code erlang-otp} archives.
   * 
   * @component role="org.codehaus.plexus.archiver.UnArchiver" roleHint="zip"
   * @required
   */
  private ZipUnArchiver zipUnArchiver;

  @SuppressWarnings("unchecked")
  public void execute() throws MojoExecutionException {
    this.libOutput.mkdirs();
    this.zipUnArchiver.setDestDirectory(this.libOutput);
    this.zipUnArchiver.setOverwrite(true);
    this.zipUnArchiver.enableLogging(new MavenPlexusLogger(getLog()));

    for (Artifact artifact : (Set<Artifact>) this.project.getArtifacts()) {
      if (artifact.getType().equals(ErlConstants.ARTIFACT_TYPE_OTP)) {
        extractArtifact(artifact, this.zipUnArchiver, this.libOutput);
      }
    }
  }

  private void extractArtifact(Artifact artifact, ZipUnArchiver unArchiver, File dest) throws MojoExecutionException {
    Log log = getLog();
    try {
      Enumeration<? extends ZipEntry> entries = new ZipFile(artifact.getFile()).entries();
      if (entries.hasMoreElements()) {
        ZipEntry firstElement = entries.nextElement();
        File cachedElement = new File(dest, firstElement.getName());
        if (!cachedElement.exists() || firstElement.getTime() != cachedElement.lastModified()) {
          log.info("Extracting artifact " + artifact.getGroupId() + ":" + artifact.getId());
          unArchiver.setSourceFile(artifact.getFile());
          unArchiver.extract();
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
