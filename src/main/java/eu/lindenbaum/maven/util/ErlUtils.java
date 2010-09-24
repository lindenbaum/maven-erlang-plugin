package eu.lindenbaum.maven.util;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
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
   */
  public static String eval(Log log, String expression) throws MojoExecutionException, MojoFailureException {
    return eval(log, expression, null);
  }

  /**
   * Evaluate an erlang expression and return the result.
   * 
   * @param log logger
   * @param expression the expression to evaluate.
   * @param libPaths list of paths to add to the path, maybe {@code null}
   * @return the output of the erl interpreter.
   */
  public static String eval(Log log, String expression, List<File> libPaths) throws MojoExecutionException,
                                                                            MojoFailureException {
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
   */
  public static String eval(final Log log, String expression, List<File> libPaths, File workingDir) throws MojoExecutionException,
                                                                                                   MojoFailureException {
    log.debug("Evaluating <<" + expression + ">>");
    final List<String> command = new LinkedList<String>();
    command.add(ErlConstants.ERL);
    if (libPaths != null) {
      for (File lib : libPaths) {
        command.add("-pa");
        command.add(lib.getAbsolutePath());
      }
    }
    command.add("-eval");
    command.add(expression);
    command.add("-noshell");
    command.add("-s");
    command.add("init");
    command.add("stop");

    return exec(command, log, workingDir, new Observer() {
      @Override
      public String handle(int exitValue, String result) throws MojoExecutionException {
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
   * {@link Observer} is notfied to process the result.
   * 
   * @param command to execute
   * @param log used to log errors
   * @param workingDir the working directory for the spawned process, maybe {@code null}
   * @param observer to be notified on process completion
   */
  public static String exec(List<String> commands, final Log log, File workingDir, Observer observer) throws MojoExecutionException,
                                                                                                     MojoFailureException {
    log.debug("Executing " + commands.toString());
    try {
      ProcessBuilder processBuilder = new ProcessBuilder(commands);
      processBuilder.directory(workingDir);
      log.debug("Working directory " + processBuilder.directory());
      Process process = processBuilder.start();
      final LinkedList<String> output = new LinkedList<String>();
      Thread gobbler1 = new Thread(new StreamGobbler(process.getInputStream(), new Processor() {
        @Override
        public void handle(String line) {
          log.info(line);
          output.addLast(line);
        }
      }));
      Thread gobbler2 = new Thread(new StreamGobbler(process.getErrorStream(), new Processor() {
        @Override
        public void handle(String line) {
          log.error(line);
        }
      }));
      gobbler1.start();
      gobbler2.start();
      process.waitFor();
      gobbler1.join();
      gobbler2.join();
      return observer.handle(process.exitValue(), output.isEmpty() ? null : output.getLast());
    }
    catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
    catch (InterruptedException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
  }
}
