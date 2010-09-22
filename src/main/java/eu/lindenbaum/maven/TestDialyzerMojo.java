package eu.lindenbaum.maven;

import java.io.File;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Run dialyzer on the generated test beam files.
 * 
 * @goal test-dialyzer
 * @phase process-test-classes
 */
public final class TestDialyzerMojo extends AbstractDialyzerMojo {
  /**
   * Directory where the test beam files reside.
   * 
   * @parameter expression="${project.build.directory}/test"
   * @required
   */
  private File testOutput;

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
  public void execute() throws MojoExecutionException {
    Log log = getLog();
    if (this.testUseDialyzer) {
      execute(this.testOutput, this.dialyzerWithDependencies);
    }
    else {
      log.info("Dialyzer is not activated for tests.");
    }
  }
}
