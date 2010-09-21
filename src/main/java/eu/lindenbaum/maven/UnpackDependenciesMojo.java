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
 * Unpack dependencies.
 * 
 * @goal unpack-dependencies
 * @phase generate-sources
 * @requiresDependencyResolution compile
 */
public final class UnpackDependenciesMojo extends AbstractMojo {
  /**
   * Project to interact with.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Directories where dependencies are unpacked. This directory contains OTP applications (name-version
   * directories, with include and ebin sub directories).
   * 
   * @parameter expression="${project.build.directory}/lib/"
   */
  private File libDirectory;

  /**
   * The Zip unarchiver.
   * 
   * @component role="org.codehaus.plexus.archiver.UnArchiver" roleHint="zip"
   * @required
   */
  private ZipUnArchiver zipUnArchiver;

  @SuppressWarnings("unchecked")
  public void execute() throws MojoExecutionException {
    this.libDirectory.mkdirs();
    this.zipUnArchiver.setDestDirectory(this.libDirectory);
    this.zipUnArchiver.setOverwrite(true);
    this.zipUnArchiver.enableLogging(new MavenPlexusLogger(getLog()));

    for (Artifact artifact : (Set<Artifact>) this.project.getArtifacts()) {
      if (artifact.getType().equals(ErlConstants.ARTIFACT_TYPE_OTP)) {
        extractArtifact(artifact, this.zipUnArchiver, this.libDirectory);
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
