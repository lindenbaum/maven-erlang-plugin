package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.LoggingUtils.logDebug;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Containing utilities related to erlang code execution.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class ErlUtils {
  /**
   * Evaluate an erlang expression and return the result.
   * 
   * @param log logger.
   * @param expression the expression to evaluate.
   * @return the output of the erl interpreter.
   * @throws MojoExecutionException if there was a problem with the erlang runtime.
   */
  public static String eval(Log log, String expression) throws MojoExecutionException {
    return eval(log, expression, null);
  }

  /**
   * Evaluate an erlang expression and return the result.
   * 
   * @param log logger
   * @param expression the expression to evaluate.
   * @param libPaths list of paths to add to the path, maybe {@code null}
   * @return the output of the erl interpreter.
   * @throws MojoExecutionException if there was a problem with the erlang runtime.
   */
  public static String eval(Log log, String expression, List<String> libPaths) throws MojoExecutionException {
    return eval(log, expression, libPaths, null);
  }

  /**
   * Evaluate an erlang expression and return the result.
   * 
   * @param log logger
   * @param expression the expression to evaluate.
   * @param libPaths list of paths to add to the path, maybe {@code null}
   * @param workingDir the working directory for the spawned process, maybe {@code null}
   * @return the output of the erl interpreter.
   * @throws MojoExecutionException if there was a problem with the erlang runtime.
   */
  public static String eval(final Log log, String expression, List<String> libPaths, File workingDir) throws MojoExecutionException {
    logDebug(log, "Evaluating <<" + expression + ">>");
    List<String> commandLine = new LinkedList<String>();
    commandLine.add(ErlConstants.ERL);
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
        logDebug(log, "Working directory " + workingDir.getAbsolutePath());
        process = runtime.exec(command, null, workingDir);
      }
      else {
        process = runtime.exec(command);
      }
      StreamGobbler error = new StreamGobbler(process.getErrorStream(), log);
      StreamGobbler output = new StreamGobbler(process.getInputStream(), log);

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
}
