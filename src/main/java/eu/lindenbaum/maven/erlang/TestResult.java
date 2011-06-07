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
   * Returns the number of passed test cases.
   * 
   * @return a number greater or equal to zero.
   */
  public int passed();

  /**
   * Returns the number of failed test cases.
   * 
   * @return a number greater or equal to zero.
   */
  public int failed();

  /**
   * Returns the number of skipped test cases.
   * 
   * @return a number greater or equal to zero.
   */
  public int skipped();

  /**
   * Returns the number of cancelled test cases.
   * 
   * @return a number greater or equal to zero.
   */
  public int cancelled();

  /**
   * Log the test output (e.g. infos/warnings/errors) using the provided logger.
   * 
   * @param log used to print the output
   */
  public void logOutput(Log log);
}
