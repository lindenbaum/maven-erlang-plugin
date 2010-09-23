package eu.lindenbaum.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * Run dialyzer on the generated test beam files.
 * 
 * @goal test-dialyzer
 * @phase process-test-classes
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestDialyzerMojo extends AbstractDialyzerMojo {
  /**
   * Setting this to {@code false} will disable the {@code dialyzer} analysis.
   * 
   * @parameter default-value=false
   */
  private boolean testUseDialyzer;

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
    if (this.testUseDialyzer) {
      execute(this.targetTest, this.dialyzerWithDependencies);
    }
    else {
      log.info("Dialyzer is not activated for tests.");
    }
  }
}
