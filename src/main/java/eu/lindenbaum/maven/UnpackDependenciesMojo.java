package eu.lindenbaum.maven;

import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.plexus.archiver.ArchiverException;
import org.codehaus.plexus.archiver.zip.ZipUnArchiver;

/**
 * Unpack dependencies.
 * 
 * @goal unpack-dependencies
 * @phase generate-sources
 * @requiresDependencyResolution compile
 */
public final class UnpackDependenciesMojo extends AbstractErlMojo {
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
    final File theLibDirectory = getLibDirectory();
    if (!theLibDirectory.exists()) {
      theLibDirectory.mkdirs();
    }
    this.zipUnArchiver.setDestDirectory(theLibDirectory);
    this.zipUnArchiver.setOverwrite(true);
    this.zipUnArchiver.enableLogging(new MavenPlexusLogger(getLog()));

    final Set<Artifact> theArtifacts = getProject().getArtifacts();
    for (Artifact theArtifact : theArtifacts) {
      if (theArtifact.getType().equals(ErlUtils.ARTIFACT_TYPE_OTP)) {
        try {
          final File theArtifactFile = theArtifact.getFile();
          final ZipFile theArtifactZip = new ZipFile(theArtifactFile);
          final Enumeration<? extends ZipEntry> theEntries = theArtifactZip.entries();
          if (!theEntries.hasMoreElements()) {
            getLog().warn("Artifact " + theArtifact.getGroupId() + ":" + theArtifact.getId() + " is empty!");
          }
          else {
            final ZipEntry theFirstEntry = theEntries.nextElement();
            final String theFirstEntryPath = theFirstEntry.getName();
            final File theFirstFile = new File(theLibDirectory, theFirstEntryPath);
            final long theFirstFileModDate;
            if (theFirstFile.exists()) {
              theFirstFileModDate = theFirstFile.lastModified();
            }
            else {
              theFirstFileModDate = 0;
            }
            final long theArchivedFirstFileModDate = theFirstEntry.getTime();
            if (theArchivedFirstFileModDate != theFirstFileModDate) {
              getLog().debug("Extracting artifact " + theArtifact.getGroupId() + ":" + theArtifact.getId());
              this.zipUnArchiver.setSourceFile(theArtifactFile);
              this.zipUnArchiver.extract();
            }
            else {
              getLog().debug("Skipping artifact " + theArtifact.getGroupId() + ":" + theArtifact.getId()
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
