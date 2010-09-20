package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.LoggingUtils.logDebug;

import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.archiver.ArchiverException;
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

  /**
   * Unpack dependencies (if required) inside the lib directory.
   * 
   * @throws MojoExecutionException if there was a problem with unpacking the dependencies.
   */
  @SuppressWarnings("unchecked")
  public void execute() throws MojoExecutionException {
    Log log = getLog();
    if (!this.libDirectory.exists()) {
      this.libDirectory.mkdirs();
    }
    this.zipUnArchiver.setDestDirectory(this.libDirectory);
    this.zipUnArchiver.setOverwrite(true);
    this.zipUnArchiver.enableLogging(new MavenPlexusLogger(log));

    Set<Artifact> artifacts = this.project.getArtifacts();
    for (Artifact artifact : artifacts) {
      if (artifact.getType().equals(ErlConstants.ARTIFACT_TYPE_OTP)) {
        try {
          File file = artifact.getFile();
          ZipFile zip = new ZipFile(file);
          Enumeration<? extends ZipEntry> entries = zip.entries();
          if (!entries.hasMoreElements()) {
            log.warn("Artifact " + artifact.getGroupId() + ":" + artifact.getId() + " is empty!");
          }
          else {
            ZipEntry entry = entries.nextElement();
            String name = entry.getName();

            File cachedArtifact = new File(this.libDirectory, name);
            long cachedModificationDate;
            if (cachedArtifact.exists()) {
              cachedModificationDate = cachedArtifact.lastModified();
            }
            else {
              cachedModificationDate = 0;
            }

            long modificationDate = entry.getTime();
            if (modificationDate != cachedModificationDate) {
              logDebug(log, "Extracting artifact " + artifact.getGroupId() + ":" + artifact.getId());
              this.zipUnArchiver.setSourceFile(file);
              this.zipUnArchiver.extract();
            }
            else {
              logDebug(log, "Skipping artifact " + artifact.getGroupId() + ":" + artifact.getId()
                            + " since it is not new.");
            }
          }
        }
        catch (IOException anException) {
          throw new MojoExecutionException(anException.getMessage(), anException);
        }
        catch (ArchiverException anException) {
          throw new MojoExecutionException(anException.getMessage(), anException);
        }
      }
    }
  }
}
