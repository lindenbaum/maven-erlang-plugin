package eu.lindenbaum.maven;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
   * Regex for the OTP application module directories.
   */
  private static final Pattern OTP_DIRECTORY_REGEX = Pattern.compile("([^-]+)-([^-]+)");

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
   * Path to the erlang installation directory.
   * 
   * @parameter
   */
  private String erlPath;

  /**
   * Cache of the application paths.
   */
  private final Map<String, String> applicationPathCache = new HashMap<String, String>();

  /**
   * Cache of the lib paths.
   */
  private List<String> libPaths;

  /**
   * Get the erlPath option.
   * 
   * @return the value of the erlPath option.
   */
  protected final String getErlPath() {
    return this.erlPath;
  }

  /**
   * Get the project.
   * 
   * @return the value of the project mojo parameter.
   */
  protected final MavenProject getProject() {
    return this.project;
  }

  /**
   * Get the libDirectory option.
   * 
   * @return the value of the libDirectory option.
   */
  protected final File getLibDirectory() {
    return this.libDirectory;
  }

  /**
   * Evaluate an erlang expression and return the result.
   * 
   * @param inExpression the expression to evaluate.
   * @return the output of the erl interpreter.
   * @throws MojoExecutionException if there was a problem with the erlang runtime.
   */
  protected String eval(String inExpression) throws MojoExecutionException {
    return ErlConstants.eval(getLog(), this.erlPath, getLibPaths(), inExpression);
  }

  /**
   * Find an include file for a given include_lib directive.
   * 
   * @param inModule module to find the include from.
   * @param inPath path to the include file.
   * @return the file or <code>null</code>.
   * @throws MojoExecutionException if there was a problem with the erlang runtime.
   */
  protected File getIncludeLibFile(String inModule, String inPath) throws MojoExecutionException {
    String theModulePath = this.applicationPathCache.get(inModule);
    if (theModulePath == null) {
      theModulePath = eval("io:format(code:lib_dir(" + inModule + ")), io:nl().");
      this.applicationPathCache.put(inModule, theModulePath);
    }
    return new File(theModulePath, inPath);
  }

  /**
   * Accessor on the path to an erlang command such as erlc, erl, dialyzer.
   * 
   * @param inCommandName name of the command.
   * @return <code>inCommandName</code> if the erlpath option wasn't specified (works if <code>$PATH</code> is
   *         configured property) or <code>erlPath/bin/inCommandName</code>.
   */
  protected String getErlCommand(String inCommandName) {
    return ErlConstants.getErlCommand(this.erlPath, inCommandName);
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

  /**
   * Return the list of the module-version/ebin/ paths in the lib directory.
   * 
   * @return a list of full paths.
   */
  protected List<String> getLibPaths() {
    if (this.libPaths == null) {
      this.libPaths = new LinkedList<String>();
      if (this.libDirectory.exists()) {
        final Map<String, String> theModuleVersions = new HashMap<String, String>();
        for (File theDirectory : this.libDirectory.listFiles()) {
          if (theDirectory.isDirectory()) {
            final File theEbinDirectory = new File(theDirectory, ErlConstants.EBIN_DIRECTORY);
            if (theEbinDirectory.exists() && theEbinDirectory.isDirectory()) {
              final String theName = theDirectory.getName();
              final Matcher theMatcher = OTP_DIRECTORY_REGEX.matcher(theName);
              final String theModuleName;
              final String theModuleVersion;
              if (theMatcher.matches()) {
                theModuleName = theMatcher.group(1);
                theModuleVersion = theMatcher.group(2);
              }
              else {
                theModuleName = theName;
                theModuleVersion = null;
              }

              if (theModuleVersions.containsKey(theModuleName)) {
                getLog().warn("Several versions for module " + theModuleName + " : we don't handle that yet");
              }
              else {
                theModuleVersions.put(theModuleName, theModuleVersion);
              }
            }
            else {
              getLog().warn("Skipping module " + theDirectory.getName()
                            + " because it has no ebin subdirectory");
            }
          }
        }

        for (Map.Entry<String, String> theEntry : theModuleVersions.entrySet()) {
          final String theModuleName = theEntry.getKey();
          final String theModuleVersion = theEntry.getValue();
          final String theModuleDir;
          if (theModuleVersion == null) {
            theModuleDir = theModuleName;
          }
          else {
            theModuleDir = theModuleName + "-" + theModuleVersion;
          }
          final File theModuleDirFile = new File(this.libDirectory, theModuleDir);
          final File theEbinDirFile = new File(theModuleDirFile, ErlConstants.EBIN_DIRECTORY);
          this.libPaths.add(theEbinDirFile.getPath());
        }
      }
    }
    return this.libPaths;
  }

  /**
   * Copy the files from a source directory to a destination directory, skipping .svn and CVS directories.
   */
  protected static void copyDirectoryStructure(File sourceDirectory, File destinationDirectory) throws IOException {
    copyDirectoryStructure(null, sourceDirectory, destinationDirectory, destinationDirectory, false);
  }

  /**
   * Code from FileUtils, modified to ignore CVS and .svn directories
   */
  protected static void copyDirectoryStructure(File sourceDirectory,
                                               File destinationDirectory,
                                               FilenameFilter inFilter) throws IOException {
    copyDirectoryStructure(inFilter, sourceDirectory, destinationDirectory, destinationDirectory, false);
  }

  protected static void copyDirectoryStructure(FilenameFilter inFilter,
                                               File sourceDirectory,
                                               File destinationDirectory,
                                               File rootDestinationDirectory,
                                               boolean onlyModifiedFiles) throws IOException {
    if (sourceDirectory == null) {
      throw new IOException("source directory can't be null.");
    }

    if (destinationDirectory == null) {
      throw new IOException("destination directory can't be null.");
    }

    if (sourceDirectory.equals(destinationDirectory)) {
      throw new IOException("source and destination are the same directory.");
    }

    if (!sourceDirectory.exists()) {
      throw new IOException("Source directory doesn't exists (" + sourceDirectory.getAbsolutePath() + ").");
    }

    final File[] files = sourceDirectory.listFiles(inFilter);
    final String sourcePath = sourceDirectory.getAbsolutePath();

    for (File file : files) {
      if (file.equals(rootDestinationDirectory)) {
        //				We don't copy the destination directory in itself
        continue;
      }

      String dest = file.getAbsolutePath();
      dest = dest.substring(sourcePath.length() + 1);

      File destination = new File(destinationDirectory, dest);

      if (file.isFile()) {
        destination = destination.getParentFile();

        if (onlyModifiedFiles) {
          FileUtils.copyFileToDirectoryIfModified(file, destination);
        }
        else {
          FileUtils.copyFileToDirectory(file, destination);
        }
      }
      else if (file.isDirectory()) {
        // Ignore .svn and CVS directories
        if (".svn".equals(file.getName())) {
          continue;
        }
        if ("CVS".equals(file.getName())) {
          continue;
        }

        if (!destination.exists() && !destination.mkdirs()) {
          throw new IOException("Could not create destination directory '" + destination.getAbsolutePath()
                                + "'.");
        }
        copyDirectoryStructure(inFilter, file, destination, rootDestinationDirectory, onlyModifiedFiles);
      }
      else {
        throw new IOException("Unknown file type: " + file.getAbsolutePath());
      }
    }
  }
}
