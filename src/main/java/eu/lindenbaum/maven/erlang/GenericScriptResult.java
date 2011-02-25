package eu.lindenbaum.maven.erlang;

import org.apache.maven.plugin.logging.Log;

/**
 * Interface representing a generic result of a {@link Script} execution
 * providing a success qualifier and output to log.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public interface GenericScriptResult {
  /**
   * Returns whether script execution was successfull.
   * 
   * @return {@code true} if script succeeded, {@code false} otherwise.
   */
  public boolean success();

  /**
   * Log the test output (e.g. infos/warnings/errors) using the provided logger.
   * 
   * @param log used to print the output
   */
  public void logOutput(Log log);
}
