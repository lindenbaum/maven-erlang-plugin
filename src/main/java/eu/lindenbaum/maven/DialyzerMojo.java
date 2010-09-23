package eu.lindenbaum.maven;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * This {@link Mojo} runs the erlang {@code dialyzer} tool on the compiled
 * artifacts found in {@link AbstractErlangMojo#targetEbin}. The
 * {@code dialyzer} can be skipped using the {@code useDialyzer} paramter in
 * the projects pom. Additionally, the user can choose to run {@code dialyzer}
 * also on the projects dependencies using the {@code dialyzerWithDependencies}
 * pom parameter. This is disabled by default since this is very slow.
 * 
 * @goal dialyzer
 * @phase process-classes
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class DialyzerMojo extends AbstractDialyzerMojo {
  /**
   * Setting this to {@code false} will disable the {@code dialyzer} analysis.
   * 
   * @parameter default-value=false
   */
  private boolean useDialyzer;

  /**
   * Setting this to {@code true} will include the projects dependencies into the {@code dialyzer} run. Note:
   * This may take very long.
   * 
   * @parameter default-value=false
   */
  private boolean dialyzerWithDependencies;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    if (this.useDialyzer) {
      execute(this.targetEbin, this.dialyzerWithDependencies);
    }
    else {
      log.info("Dialyzer is not activated.");
    }
  }
}
