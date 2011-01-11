package eu.lindenbaum.maven.mojo;

import java.io.IOException;
import java.util.ArrayList;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.NodeShutdownHook;
import eu.lindenbaum.maven.erlang.PurgeModulesScript;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
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
  protected void execute(final Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    String nodeName = p.node();
    String nodeCookie = p.cookie();
    OtpPeer peer = new OtpPeer(nodeName);
    try {
      try {
        String startupName = "maven-erlang-plugin-startup-" + System.nanoTime();
        OtpSelf self = nodeCookie != null ? new OtpSelf(startupName, nodeCookie) : new OtpSelf(startupName);
        self.connect(peer);
        log.debug("Node " + peer + " is already running.");
      }
      catch (IOException e) {
        log.debug("starting " + peer + ".");
        ArrayList<String> command = new ArrayList<String>();
        command.add(ErlConstants.ERL);
        command.add("-boot");
        command.add("start_sasl");
        command.add("-name");
        command.add(peer.node());
        command.add("-detached");
        Process process = new ProcessBuilder(command).start();
        if (process.waitFor() != 0) {
          throw new MojoExecutionException("Failed to start " + peer + ".");
        }
        log.debug("Node " + peer + " sucessfully started.");
      }
      if (this.shutdownNode) {
        try {
          Runtime.getRuntime().addShutdownHook(NodeShutdownHook.get(nodeName, nodeCookie));
        }
        catch (IllegalArgumentException e1) {
          log.debug("shutdown hook already registered.");
        }
      }
      else {
        log.info("Node " + peer + " will not be shutdown automatically.");
        log.info("To shutdown the node run 'mvn erlang:initialize -DshutdownNode=true'");
      }

      // clean up dynamically loaded modules on backend from previous runs
      Script<Void> purgeScript = new PurgeModulesScript();
      MavenSelf.get(nodeCookie).exec(nodeName, purgeScript);
    }
    catch (IOException e) {
      throw new MojoExecutionException("Failed to start " + peer + ".", e);
    }
    catch (OtpAuthException e) {
      throw new MojoExecutionException("Failed to connect to " + peer + ".", e);
    }
    catch (InterruptedException e) {
      throw new MojoExecutionException("Failed to start " + peer + ".", e);
    }
  }
}
