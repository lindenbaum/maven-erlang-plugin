package eu.lindenbaum.maven.erlang;

import org.apache.maven.plugin.logging.Log;

/**
 * Interface representing the result returned by the {@link ProfilingScript}.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @since 2.1.0
 */
public interface ProfilingResult {
  /**
   * Log the profiling test output (e.g. infos/warnings/errors) using the provided logger.
   * 
   * @param log used to print the output
   */
  void logOutput(Log log);

  /**
   * Returns whether all profiled tests passed, and that no test cases failed.
   * 
   * @return {@code true} if all tests passed, {@code false} otherwise.
   */
  boolean testsPassed();
}
