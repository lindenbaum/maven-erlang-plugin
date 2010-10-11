package eu.lindenbaum.maven.util;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * A class interested in the result of an {@link Process}.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface Observer {
  /**
   * Called when the observed process exited.
   * 
   * @param exitValue of the process
   * @param result the last output line of the process
   * @return a implementation specific return value
   * @throws MojoExecutionException
   * @throws MojoFailureException
   */
  public String handle(int exitValue, String result) throws MojoExecutionException, MojoFailureException;
}
