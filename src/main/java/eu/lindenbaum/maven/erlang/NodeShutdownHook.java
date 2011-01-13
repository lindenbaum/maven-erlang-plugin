package eu.lindenbaum.maven.erlang;

import java.util.HashMap;
import java.util.Map;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangList;

import eu.lindenbaum.maven.util.MavenUtils;

/**
 * Provides JVM unique shutdown hooks that stop erlang backend nodes.
 * 
 * @author Tobias Schlager tobias.schlager@lindenbaum.eu
 */
public final class NodeShutdownHook extends Thread {
  private static final Map<String, NodeShutdownHook> instances = new HashMap<String, NodeShutdownHook>();

  private final String nodeName;
  private volatile String nodeCookie;

  private NodeShutdownHook(String nodeName, String nodeCookie) {
    this.nodeName = nodeName;
    this.nodeCookie = nodeCookie;
  }

  /**
   * Returns an JVM unique instance of a {@link NodeShutdownHook} per node name.
   * Shutdown hook registration is not done automatically and must be done in a
   * separate step.
   * 
   * @param nodeName name of the erlang node to shut down
   * @param nodeCookie cookie of the erlang node to shut down
   * @return an instance of {@link NodeShutdownHook}, never <code>null</code>
   */
  public static NodeShutdownHook get(String nodeName, String nodeCookie) {
    synchronized (instances) {
      NodeShutdownHook shutdownHook = instances.get(nodeName);
      if (shutdownHook == null) {
        shutdownHook = new NodeShutdownHook(nodeName, nodeCookie);
        instances.put(nodeName, shutdownHook);
      }
      else {
        shutdownHook.nodeCookie = nodeCookie;
      }
      return shutdownHook;
    }
  }

  /**
   * Shuts down the node refered to by {@link #nodeName} using
   * <code>erlang:halt/0</code>.
   */
  @Override
  public void run() {
    try {
      OtpConnection connection = MavenSelf.get(this.nodeCookie).connect(this.nodeName);
      connection.sendRPC("erlang", "halt", new OtpErlangList());
      System.out.println("[INFO] Successfully shut down '" + this.nodeName + "'");
    }
    catch (Exception e) {
      System.out.println("[ERROR] Failed to shutdown '" + this.nodeName + "'");
    }
    System.out.println("[INFO] " + MavenUtils.SEPARATOR);
  }
}
