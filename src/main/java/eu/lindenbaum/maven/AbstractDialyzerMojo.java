package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlUtils.exec;

import java.util.List;

import eu.lindenbaum.maven.util.Observer;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * Base class for {@link Mojo}s using the {@code dialyzer tool}.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Timo Koepke <timo.koepke@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
abstract class AbstractDialyzerMojo extends AbstractErlangMojo {
  /**
   * Setting this to {@code true} will break the build when a {@code dialyzer}
   * run returns warnings.
   * 
   * @parameter expression=${dialyzerWarningsAreErrors} default-value=false
   */
  boolean dialyzerWarningsAreErrors;

  /**
   * Starts a process evaluating a specific dialyzer command line.
   * 
   * @param command the command line to execute
   * @throws MojoExecutionException
   * @throws MojoFailureException
   */
  protected void dialyze(List<String> command) throws MojoExecutionException, MojoFailureException {
    exec(command, getLog(), null, new Observer() {
      @Override
      public String handle(int exitValue, String result) throws MojoExecutionException, MojoFailureException {
        if (exitValue == 2) {
          if (AbstractDialyzerMojo.this.dialyzerWarningsAreErrors) {
            throw new MojoFailureException("Dialyzer emitted warnings.");
          }
        }
        else if (exitValue == 1) {
          throw new MojoFailureException("Dialyzer found errors.");
        }
        else if (exitValue != 0) {
          throw new MojoExecutionException("Dialyzer returned with " + exitValue);
        }
        return null;
      }
    });
  }
}
