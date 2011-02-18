package eu.lindenbaum.maven.mojo.app;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * {@link Mojo} that starts the a test erlang node used as a backend for rpcs
 * made by the plugin. The node will only be started if it is not already
 * running. The node will be shutdown when the executing JVM exits. This is done
 * by a {@link Runtime#addShutdownHook(Thread)} which will only be added
 * <b>once</b> each JVM execution.
 * 
 * @goal test-initialize
 * @phase generate-test-sources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class TestInitializer extends ErlangMojo {
  /**
   * Setting this to {@code true} will skip the test initialization.
   * 
   * @parameter expression="${skipTests}" default-value=false
   */
  private boolean skipTests;

  /**
   * Setting this to {@code false} will leave the plugins test backend node up
   * and running even if the executing jvm exits.
   * 
   * @parameter expression="${shutdownTestNode}" default-value=true
   */
  private volatile boolean shutdownTestNode = true;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    if (this.skipTests) {
      log.info("Test initialization is skipped.");
      return;
    }
    ErlUtils.startBackend(log,
                          "erlang:test-initialize -DshutdownTestNode=true",
                          p.testNode(),
                          p.testCookie(),
                          this.shutdownTestNode);
  }
}
