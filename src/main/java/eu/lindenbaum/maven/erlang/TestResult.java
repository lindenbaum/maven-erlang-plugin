package eu.lindenbaum.maven.erlang;

import org.apache.maven.plugin.logging.Log;

/**
 * Interface representing the result returned by the {@link TestScript}.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public interface TestResult {
  /**
   * Returns whether all tests passed, no test cases failed.
   * 
   * @return {@code true} if all tests passed, {@code false} otherwise.
   */
  public boolean testsPassed();

  /**
   * Log the test output (e.g. infos/warnings/errors) using the provided logger.
   * 
   * @param log used to print the output
   */
  public void logOutput(Log log);
}
