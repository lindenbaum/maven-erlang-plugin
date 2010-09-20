package eu.lindenbaum.maven.util;

import java.io.File;
import java.io.FilenameFilter;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Defines Erlang related constants.
 */
public final class ErlConstants {
  /**
   * Name of the erlang interpreter binary.
   */
  public static final String ERL = "erl";

  /**
   * Name of the erlang compiler.
   */
  public static final String ERLC = "erlc";

  /**
   * Name of the dialyzer tool.
   */
  public static final String DIALYZER = "dialyzer";

  /**
   * Suffix for erlang source files.
   */
  public static final String ERL_SUFFIX = ".erl";

  /**
   * Suffix for erlang header files.
   */
  public static final String HRL_SUFFIX = ".hrl";

  /**
   * Suffix for application resource files.
   */
  public static final String APP_SUFFIX = ".app";

  /**
   * Suffix for application upgrade files.
   */
  public static final String APPUP_SUFFIX = ".appup";

  /**
   * Suffix for mibs.
   */
  public static final String MIB_SUFFIX = ".mib";

  /**
   * Suffix for funcs (mibs handlers).
   */
  public static final String FUNCS_SUFFIX = ".funcs";

  /**
   * Suffix for mibs binaries
   */
  public static final String BIN_SUFFIX = ".bin";

  /**
   * Suffix for erlang binary files.
   */
  public static final String BEAM_SUFFIX = ".beam";

  /**
   * Suffix for rel files.
   */
  public static final String REL_SUFFIX = ".rel";

  /**
   * Name of the coverdata binary (coverdata) file.
   */
  public static final String COVERDATA_BIN = "coverdata.coverdata";

  /**
   * Type of artifacts for applications, i.e. zip archive containing an erlang-otp application.
   */
  public static final String ARTIFACT_TYPE_OTP = "erlang-otp";

  /**
   * Type of artifacts for releases, i.e. tar gz archive containing an erlang-otp release.
   */
  public static final String ARTIFACT_TYPE_REL = "erlang-rel";

  /**
   * Name of the directory that contains the beam files.
   */
  public static final String EBIN_DIRECTORY = "ebin";

  /**
   * Name of the directory that contains the include files.
   */
  public static final String INCLUDE_DIRECTORY = "include";

  /**
   * Filename filter to filter source files (.erl & .hrl).
   */
  public static final FilenameFilter SOURCE_FILENAME_FILTER = new FilenameFilter() {
    public boolean accept(File inDir, String inName) {
      return isSourceFile(inName);
    }
  };

  /**
   * Return true if the given file is a source file (and therefore should be included in the src/ directory in
   * the package). Uses the file suffix.
   * 
   * @param filename name of the considered file.
   * @return <code>true</code> if the file is a source file.
   */
  public static boolean isSourceFile(String filename) {
    return filename != null && (filename.endsWith(HRL_SUFFIX) || filename.endsWith(ERL_SUFFIX));
  }

  /**
   * Generate the documentation with edoc for an OTP application.
   * 
   * @param log Reference on the Maven log object.
   * @param edocOptions Additional edoc options, or <code>null</code>
   * @param appName name of the OTP application.
   * @param sourcePath path to the source directory.
   * @param appDirectory directory where the .app file can be found.
   * @param outputPath path to the output directory.
   * @throws MojoExecutionException If a problem occurs with the erl runtime.
   */
  public static void generateEdocAppDocumentation(Log log,
                                                  String[] edocOptions,
                                                  String appName,
                                                  File sourcePath,
                                                  File appDirectory,
                                                  File outputPath) throws MojoExecutionException {
    generateEdocDocumentation(log, edocOptions, appName, sourcePath, appDirectory, outputPath);
  }

  /**
   * Generate the documentation with edoc for a set of files.
   * 
   * @param log Reference on the Maven log object.
   * @param edocOptions Additional edoc options, or <code>null</code>
   * @param sourcePath path to the source directory.
   * @param outputPath path to the output directory.
   * @throws MojoExecutionException If a problem occurs with the erl runtime.
   */
  public static void generateEdocFilesDocumentation(Log log,
                                                    String[] edocOptions,
                                                    File sourcePath,
                                                    File outputPath) throws MojoExecutionException {
    generateEdocDocumentation(log, edocOptions, null, sourcePath, null, outputPath);
  }

  /**
   * Generate the documentation with edoc for a set of files.
   * 
   * @param log Reference on the Maven log object.
   * @param edocOptions Additional edoc options, or <code>null</code>
   * @param appName name of the OTP application or <code>null</code> to generate the documentation from files.
   * @param sourcePath path to the source directory.
   * @param appDirectory directory where the .app file can be found.
   * @param outputPath path to the output directory.
   * @throws MojoExecutionException If a problem occurs with the erl runtime.
   */
  private static void generateEdocDocumentation(Log log,
                                                String[] edocOptions,
                                                String appName,
                                                File sourcePath,
                                                File appDirectory,
                                                File outputPath) throws MojoExecutionException {
    final StringBuilder theEdocLineBuffer = new StringBuilder();
    if (appName != null) {
      theEdocLineBuffer.append("edoc:application(").append(appName).append(",\"");
      theEdocLineBuffer.append(appDirectory.getPath()).append("\",");
    }
    else {
      theEdocLineBuffer.append("edoc:files([");
      // all source files.
      final File[] theSourceFiles = sourcePath.listFiles(SOURCE_FILENAME_FILTER);
      boolean first = true;
      for (final File theSourceFile : theSourceFiles) {
        if (!first) {
          theEdocLineBuffer.append(", ");
        }
        first = false;
        theEdocLineBuffer.append("\"").append(theSourceFile.getPath()).append("\"");
      }
      theEdocLineBuffer.append("],");
    }
    theEdocLineBuffer.append("[{dir, \"")
                     .append(outputPath.getPath())
                     .append("\"},")
                     .append(" {source_path, [\"")
                     .append(sourcePath.getPath())
                     .append("\"]}");
    // options
    if (edocOptions != null) {
      for (final String theOption : edocOptions) {
        theEdocLineBuffer.append(",").append(theOption);
      }
    }
    theEdocLineBuffer.append("]).");

    ErlUtils.eval(log, theEdocLineBuffer.toString());
  }

  /**
   * Get the OTP application name from the applicationResourceFile option (if present) or the artifactId
   * (replacing dashes with underscores).
   * 
   * @param applicationResourceFile application resource file option (can be <code>null</code>).
   * @param artifactId project artifact Id.
   * @return the OTP application name.
   * @throws MojoExecutionException if the applicationResourceFile option is invalid.
   */
  public static String getApplicationName(String applicationResourceFile, String artifactId) throws MojoExecutionException {
    final String theApplicationName;
    if (applicationResourceFile == null) {
      // Try to guess the application resource file.
      theApplicationName = artifactId.replace('-', '_');
    }
    else {
      if (!applicationResourceFile.endsWith(".app")) {
        throw new MojoExecutionException("Illegal applicationResourceFile parameter. It must end with .app");
      }
      theApplicationName = applicationResourceFile.substring(0, applicationResourceFile.length() - 4);
    }
    return theApplicationName;
  }
}
