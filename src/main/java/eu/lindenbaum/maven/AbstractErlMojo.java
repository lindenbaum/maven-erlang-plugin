package eu.lindenbaum.maven;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.FileUtils;

/**
 * Base-class for Erl and Erlc-based Mojos.
 * 
 * @author Paul Guyot
 */
abstract class AbstractErlMojo extends AbstractMojo {

  /**
   * Project to interact with.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Get the project.
   * 
   * @return the value of the project mojo parameter.
   */
  protected final MavenProject getProject() {
    return this.project;
  }

  /**
   * Copy .mib files.
   * 
   * @param inInputDirectory directory with source files.
   * @param inOutputDirectory directory where to copy files.
   * @return the number of files actually copied.
   * @throws MojoExecutionException if there was a problem with the copy.
   */
  protected int copyMibFiles(final File inInputDirectory, final File inOutputDirectory) throws MojoExecutionException {
    final int nbMib = copyTextFilesWithSubstitutions(inInputDirectory,
                                                     inOutputDirectory,
                                                     ErlConstants.MIB_SUFFIX,
                                                     null);
    final int nbFuncs = copyTextFilesWithSubstitutions(inInputDirectory,
                                                       inOutputDirectory,
                                                       ErlConstants.FUNCS_SUFFIX,
                                                       null);
    return nbMib + nbFuncs;
  }

  /**
   * Copy .app resource files.
   * 
   * @param inInputDirectory directory with source files.
   * @param inOutputDirectory directory where to copy files.
   * @return the number of files actually copied.
   * @throws MojoExecutionException if there was a problem with the copy.
   */
  protected int copyAppFiles(final File inInputDirectory, final File inOutputDirectory) throws MojoExecutionException {
    final Map<String, String> theAppSubstitutions = new HashMap<String, String>();
    theAppSubstitutions.put("\\?APP_VERSION", "\"" + this.project.getVersion() + "\"");
    final int nbAppFiles = copyTextFilesWithSubstitutions(inInputDirectory,
                                                          inOutputDirectory,
                                                          ErlConstants.APP_SUFFIX,
                                                          theAppSubstitutions);
    final int nbAppUpFiles = copyTextFilesWithSubstitutions(inInputDirectory,
                                                            inOutputDirectory,
                                                            ErlConstants.APPUP_SUFFIX,
                                                            theAppSubstitutions);
    return nbAppFiles + nbAppUpFiles;
  }

  /**
   * Copy files with a given suffix, and optionally substitute values.
   * 
   * @param inInputDirectory directory with source files.
   * @param inOutputDirectory directory where to copy files.
   * @param inSuffix suffix of the files to copy.
   * @param inSubstitution substitutions.
   * @return the number of files actually copied.
   * @throws MojoExecutionException if there was a problem with the copy.
   */
  protected int copyTextFilesWithSubstitutions(final File inInputDirectory,
                                               final File inOutputDirectory,
                                               final String inSuffix,
                                               final Map<String, String> inSubstitutions) throws MojoExecutionException {
    // Generate the list of input files that need to be compiled.
    final File[] theFiles = inInputDirectory.listFiles(new FilenameFilter() {
      public boolean accept(File inDir, String inName) {
        return inName != null && inName.endsWith(inSuffix);
      }
    });

    int nbFiles;
    if (theFiles == null) {
      nbFiles = 0;
    }
    else {
      nbFiles = theFiles.length;
      if (nbFiles > 0) {
        // Create the output directory if required.
        if (!inOutputDirectory.exists()) {
          inOutputDirectory.mkdirs();
        }

        nbFiles = 0;
        for (File theFile : theFiles) {
          if (copyTextFileWithSubstitutions(theFile, inOutputDirectory, inSubstitutions)) {
            nbFiles++;
          }
        }

        if (nbFiles > 0) {
          getLog().info("Copied " + nbFiles + " " + inSuffix + " file(s) to " + inOutputDirectory.getPath());
        }
      }
    }

    return nbFiles;
  }

  /**
   * Copy a single text file and optionally substitute values.
   * 
   * @param inSourceFile file to copy.
   * @param inOutputDirectory directory where to copy the file.
   * @param inSubstitution substitutions.
   * @return true if the file was copied, false otherwise.
   * @throws MojoExecutionException if there was a problem with the copy.
   */
  protected boolean copyTextFileWithSubstitutions(final File inSourceFile,
                                                  final File inOutputDirectory,
                                                  final Map<String, String> inSubstitutions) throws MojoExecutionException {
    final boolean theResult;
    // Check if the file needs to be copied.
    final File theTargetFile = new File(inOutputDirectory, inSourceFile.getName());
    try {
      if (!theTargetFile.exists() || theTargetFile.lastModified() < inSourceFile.lastModified()) {
        theResult = true;
        String theData = FileUtils.fileRead(inSourceFile, "UTF-8");
        if (inSubstitutions != null) {
          for (Map.Entry<String, String> substitution : inSubstitutions.entrySet()) {
            theData = theData.replaceAll(substitution.getKey(), substitution.getValue());
          }
        }
        FileUtils.fileWrite(theTargetFile.getPath(), "UTF-8", theData);
      }
      else {
        theResult = false;
      }
    }
    catch (IOException anException) {
      throw new MojoExecutionException(anException.getMessage(), anException);
    }

    return theResult;
  }
}
