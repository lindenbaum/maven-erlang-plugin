package eu.lindenbaum.maven.util;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Erlang related utilities.
 */
public final class ErlUtils {
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
   * Logs a message using {@link Log#debug(CharSequence)} in case debug logging is enabled.
   * 
   * @param log logger to use
   * @param message message to log
   */
  public static void logDebug(Log log, String message) {
    if (log.isDebugEnabled()) {
      log.debug(message);
    }
  }

  /**
   * Evaluate an erlang expression and return the result.
   * 
   * @param log logger.
   * @param erlPath value of the erlPath option.
   * @param expression the expression to evaluate.
   * @return the output of the erl interpreter.
   * @throws MojoExecutionException if there was a problem with the erlang runtime.
   */
  public static String eval(Log log, String erlPath, String expression) throws MojoExecutionException {
    return eval(log, erlPath, null, expression);
  }

  /**
   * Evaluate an erlang expression and return the result.
   * 
   * @param log logger
   * @param erlPath value of the erlPath option.
   * @param libPaths list of paths to add to the path, or {@code null}
   * @param expression the expression to evaluate.
   * @return the output of the erl interpreter.
   * @throws MojoExecutionException if there was a problem with the erlang runtime.
   */
  public static String eval(Log log, String erlPath, List<String> libPaths, String expression) throws MojoExecutionException {
    return eval(log, erlPath, libPaths, null, expression);
  }

  /**
   * Evaluate an erlang expression and return the result.
   * 
   * @param log logger
   * @param erlPath value of the erlPath option.
   * @param libPaths list of paths to add to the path, or {@code null}
   * @param workingDir the working directory for the spawned process
   * @param expression the expression to evaluate.
   * @return the output of the erl interpreter.
   * @throws MojoExecutionException if there was a problem with the erlang runtime.
   */
  public static String eval(final Log log,
                            String erlPath,
                            List<String> libPaths,
                            File workingDir,
                            String expression) throws MojoExecutionException {
    logDebug(log, "Evaluating <<" + expression + ">>");
    List<String> commandLine = new LinkedList<String>();
    commandLine.add(getErlCommand(erlPath, ERL));
    if (libPaths != null) {
      for (String theLibPath : libPaths) {
        commandLine.add("-pa");
        commandLine.add(theLibPath);
      }
    }
    commandLine.add("-eval");
    commandLine.add(expression);
    commandLine.add("-noshell");
    commandLine.add("-s");
    commandLine.add("init");
    commandLine.add("stop");

    final String[] command = commandLine.toArray(new String[0]);
    return exec(command, log, workingDir, new ProcessListener() {
      @Override
      public String processCompleted(int exitValue, List<String> processOutput) throws MojoExecutionException {
        String result = "";
        if (!processOutput.isEmpty()) {
          result = processOutput.get(processOutput.size() - 1);
        }
        if (exitValue != 0) {
          log.error("Process returned: " + exitValue + " result is: " + result);
          throw new MojoExecutionException("Error evaluating expression " + command);
        }
        return result;
      }
    });
  }

  /**
   * Executes the given command array in the given working directory. After process completion the given
   * {@link ProcessListener} is notfied to process the result.
   * 
   * @param command to execute
   * @param log used to log errors
   * @param workingDir the working directory for the spawned process, maybe {@code null}
   * @param listener to be notified on process completion
   * @throws MojoExecutionException in case of process failure
   */
  public static String exec(String[] command, Log log, File workingDir, ProcessListener listener) throws MojoExecutionException {
    Runtime runtime = Runtime.getRuntime();
    logDebug(log, "Executing " + Arrays.asList(command).toString());

    try {
      final Process process;
      if (workingDir != null) {
        logDebug(log, "working directory for process is " + workingDir.getAbsolutePath());
        process = runtime.exec(command, null, workingDir);
      }
      else {
        process = runtime.exec(command);
      }
      StreamGobbler error = new StreamGobbler(process.getErrorStream());
      StreamGobbler output = new StreamGobbler(process.getInputStream());

      new Thread(error).start();
      new Thread(output).start();

      process.waitFor();

      for (String line : error.getLines()) {
        log.error(line);
      }
      return listener.processCompleted(process.exitValue(), output.getLines());
    }
    catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
    catch (InterruptedException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
  }

  /**
   * Accessor on the path to an erlang command such as erlc, erl, dialyzer.
   * 
   * @param erlPath value of the erlPath option.
   * @param commandName name of the command.
   * @return <code>inCommandName</code> if the erlpath option wasn't specified (works if <code>$PATH</code> is
   *         configured property) or <code>erlPath/bin/inCommandName</code>.
   */
  public static String getErlCommand(String erlPath, String commandName) {
    final String result;
    if (erlPath == null) {
      result = commandName;
    }
    else {
      result = new File(erlPath + File.separatorChar + "bin" + File.separatorChar + commandName).getPath();
    }
    return result;
  }

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
   * @param erlPath Path to erl binary or <code>null</code> if it's in $PATH
   * @param edocOptions Additional edoc options, or <code>null</code>
   * @param appName name of the OTP application.
   * @param sourcePath path to the source directory.
   * @param appDirectory directory where the .app file can be found.
   * @param outputPath path to the output directory.
   * @throws MojoExecutionException If a problem occurs with the erl runtime.
   */
  public static void generateEdocAppDocumentation(Log log,
                                                  String erlPath,
                                                  String[] edocOptions,
                                                  String appName,
                                                  File sourcePath,
                                                  File appDirectory,
                                                  File outputPath) throws MojoExecutionException {
    generateEdocDocumentation(log, erlPath, edocOptions, appName, sourcePath, appDirectory, outputPath);
  }

  /**
   * Generate the documentation with edoc for a set of files.
   * 
   * @param log Reference on the Maven log object.
   * @param erlPath Path to erl binary or <code>null</code> if it's in $PATH
   * @param edocOptions Additional edoc options, or <code>null</code>
   * @param sourcePath path to the source directory.
   * @param outputPath path to the output directory.
   * @throws MojoExecutionException If a problem occurs with the erl runtime.
   */
  public static void generateEdocFilesDocumentation(Log log,
                                                    String erlPath,
                                                    String[] edocOptions,
                                                    File sourcePath,
                                                    File outputPath) throws MojoExecutionException {
    generateEdocDocumentation(log, erlPath, edocOptions, null, sourcePath, null, outputPath);
  }

  /**
   * Generate the documentation with edoc for a set of files.
   * 
   * @param log Reference on the Maven log object.
   * @param erlPath Path to erl binary or <code>null</code> if it's in $PATH
   * @param edocOptions Additional edoc options, or <code>null</code>
   * @param appName name of the OTP application or <code>null</code> to generate the documentation from files.
   * @param sourcePath path to the source directory.
   * @param appDirectory directory where the .app file can be found.
   * @param outputPath path to the output directory.
   * @throws MojoExecutionException If a problem occurs with the erl runtime.
   */
  private static void generateEdocDocumentation(Log log,
                                                String erlPath,
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

    eval(log, erlPath, theEdocLineBuffer.toString());
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
