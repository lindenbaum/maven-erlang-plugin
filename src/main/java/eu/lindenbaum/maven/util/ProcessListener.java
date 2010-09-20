package eu.lindenbaum.maven.util;

import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A listener interested in a specific process (completion). 
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu> 
 */
public interface ProcessListener {
  /**
   * Called when the corresponding process completed.
   * 
   * @param exitValue of the process
   * @param processOutput the output lines of the process
   * @return a implementation specific return value
   * @throws MojoExecutionException
   */
  public String processCompleted(int exitValue, List<String> processOutput) throws MojoExecutionException;
}
