package eu.lindenbaum.maven.mojo;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * {@link Mojo} that starts the an erlang node used as a backend for rpcs made
 * by the plugin. The node will only be started if it is not already running.
 * The node will be shutdown when the executing JVM exits. This is done by a
 * {@link Runtime#addShutdownHook(Thread)} which will only be added <b>once</b>
 * each JVM execution.
 * 
 * @goal initialize
 * @phase initialize
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class BackendInitializer extends ErlangMojo {
  /**
   * Setting this to {@code false} will leave the plugins backend node up and
   * running even if the executing jvm exits.
   * 
   * @parameter expression="${shutdownNode}" default-value=true
   */
  private volatile boolean shutdownNode = true;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    ErlUtils.startBackend(log,
                          "erlang:initialize -DshutdownNode=true",
                          p.node(),
                          p.cookie(),
                          this.shutdownNode);
  }
}
