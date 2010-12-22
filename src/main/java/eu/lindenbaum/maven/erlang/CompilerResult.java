package eu.lindenbaum.maven.erlang;

import org.apache.maven.plugin.logging.Log;

/**
 * Interface representing the result returned by the {@link BeamCompilerScript}.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface CompilerResult {
  /**
   * Returns the failed compilation unit.
   * 
   * @return A string containing the failed compilation unit or {@code null} if
   *         compilation succeeded.
   */
  public String getFailed();

  /**
   * Log the compiler output (e.g. warnings/errors) using the provided logger.
   * 
   * @param log used to print the output
   */
  public void logOutput(Log log);
}
