package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A wrapper around an {@link OtpSelf} node that acts as a connection cache for
 * destination erlang nodes. Instantiation is guarded by the singleton pattern.
 * To retrieve an instance call {@link MavenSelf#get()}. Connections retrieved
 * by {@link MavenSelf#connect(OtpPeer)} are cached in order to return an
 * already established connection. Thus this method can be called multiple
 * times.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class MavenSelf {
  private static final int MAX_RETRIES = 10;

  private static final String script = //
  "    C_O_D_E_P_A_T_H_S_ = %s," + //
      "code:add_pathsa(C_O_D_E_P_A_T_H_S_)," + //
      "_B_E_F_O_R_E_ = code:all_loaded()," + //
      "_R_E_S_U_L_T_ = try" + //
      "                    %s " + //
      "                catch" + //
      "                    _C_:_R_ ->" + //
      "                        {exception, _C_, _R_, erlang:get_stacktrace()}" + //
      "                end," + //
      "[code:del_path(_P_A_T_H_) || _P_A_T_H_ <- C_O_D_E_P_A_T_H_S_]," + //
      "_P_U_R_G_E_ = code:all_loaded() -- _B_E_F_O_R_E_," + //
      "[code:purge(_M_O_D_) || {_M_O_D_, _} <- _P_U_R_G_E_]," + //
      "[code:delete(_M_O_D_) || {_M_O_D_, _} <- _P_U_R_G_E_]," + //
      "[code:purge(_M_O_D_) || {_M_O_D_, _} <- _P_U_R_G_E_]," + //
      "case _R_E_S_U_L_T_ of" + //
      "    {exception, _C_L_A_S_S_, _R_E_A_S_O_N_, _S_T_A_C_K_} ->" + //
      "        erlang:raise(_C_L_A_S_S_, _R_E_A_S_O_N_, _S_T_A_C_K_);" + //
      "    _ -> _R_E_S_U_L_T_ " + // do not kill the whitespace at the end of this line!!!
      "end.";

  private static final AtomicLong serial = new AtomicLong(0L);
  private static final Map<String, MavenSelf> instances = new HashMap<String, MavenSelf>();

  private final OtpSelf self;
  private final Map<String, OtpConnection> connections;

  private MavenSelf(OtpSelf self) {
    this.self = self;
    this.connections = new HashMap<String, OtpConnection>();
  }

  /**
   * Returns a unique instance of {@link MavenSelf} per cookie using the
   * singleton pattern.
   * 
   * @param c the cookie to use for this java node
   * @return an instance of {@link MavenSelf}, never {@code null}
   * @throws MojoExecutionException in case the instance cannot be created
   */
  public static MavenSelf get(String cookie) throws MojoExecutionException {
    String c = cookie != null ? cookie : "";
    MavenSelf self = instances.get(c);
    if (self == null) {
      try {
        String name = "maven-erlang-plugin-frontend-" + serial.getAndIncrement();
        OtpSelf otpSelf = c.isEmpty() ? new OtpSelf(name) : new OtpSelf(name, c);
        self = new MavenSelf(otpSelf);
        instances.put(c, self);
      }
      catch (IOException e) {
        throw new MojoExecutionException("failed to create self node for cookie '" + c + "'", e);
      }
    }
    return self;
  }

  /**
   * Establishes an {@link OtpConnection} between this node and a specific
   * {@link OtpPeer}. The returned connection may be an already existing, cached
   * connection.
   * 
   * @param peer to connect to
   * @return an {@link OtpConnection} that may be used for rpc communication
   * @throws MojoExecutionException in case the connection cannot be established
   */
  public OtpConnection connect(String peer) throws MojoExecutionException {
    OtpConnection connection = this.connections.get(peer);
    if (connection == null) {
      String msg = null;
      try {
        for (int i = 0; i < MAX_RETRIES; ++i) {
          try {
            connection = this.self.connect(new OtpPeer(peer));
            this.connections.put(peer, connection);
            break;
          }
          catch (IOException e) {
            msg = e.getMessage();
            Thread.sleep(500L);
          }
        }
      }
      catch (OtpAuthException e) {
        msg = e.getMessage();
        throw new MojoExecutionException("failed to connect to " + peer + ": " + msg);
      }
      catch (InterruptedException e) {
        msg = e.getMessage();
        throw new MojoExecutionException("failed to connect to " + peer + ": " + msg);
      }
      if (connection == null) {
        msg = " after " + MAX_RETRIES + " retries: " + msg;
        throw new MojoExecutionException("failed to connect to " + peer + msg);
      }
    }
    return connection;
  }

  /**
   * Executes a {@link Script} on a specific remote erlang node using RPC. A
   * connection to the remote node will be established if necessary. The given
   * code paths will be added before script execution and are guaranteed to be
   * removed when the script ends (abnormally). Additionally, all dynamically
   * loaded modules will be purged on the remote node when the script exits.
   * 
   * @param peer to evaluate the {@link Script} on
   * @param script to evaluate
   * @param codePaths a list of paths needed for the script to run
   * @return the processed result of the {@link Script}
   * @throws MojoExecutionException
   */
  public <T> T eval(String peer, Script<T> script, List<File> codePaths) throws MojoExecutionException {
    String scriptString = script.get().trim();
    if (scriptString.endsWith(".")) {
      scriptString = scriptString.substring(0, scriptString.length() - 1);
    }
    String pathsString = ErlUtils.toFileList(codePaths, "\"", "\"");
    String toEval = String.format(MavenSelf.script, pathsString, scriptString);
    return script.handle(eval(peer, toEval));
  }

  /**
   * Executes a {@link Script} on a specific remote erlang node using RPC. A
   * connection to the remote node will be established if necessary. NOTE: This
   * will <b>not</b> automatically purge dynamically loaded modules neither will
   * it cleanup the code path of the backend node's code server.
   * 
   * @param peer to evaluate the {@link Script} on
   * @param script to evaluate
   * @return the processed result of the {@link Script}
   * @throws MojoExecutionException
   */
  public <T> T exec(String peer, Script<T> script) throws MojoExecutionException {
    return script.handle(eval(peer, script.get()));
  }

  /**
   * Removes the cached connection to the given peer.
   * 
   * @param peer to remove connections for
   */
  private void removeConnection(String peer) {
    this.connections.remove(peer);
  }

  /**
   * Executes an erlang script on a specific remote erlang node using RPC. A
   * connection to the remote node will be established if necessary.
   * 
   * @param peer to evaluate the expression on
   * @param expression to evaluate
   * @return the result term of the expression
   * @throws MojoExecutionException
   */
  private OtpErlangObject eval(String peer, String expression) throws MojoExecutionException {
    OtpConnection connection = connect(peer);
    try {
      connection.sendRPC("erl_eval", "new_bindings", new OtpErlangList());
      OtpErlangObject bindings = connection.receiveRPC();

      connection.sendRPC("erl_scan", "string", new OtpErlangList(new OtpErlangString(expression)));
      OtpErlangTuple result = (OtpErlangTuple) connection.receiveRPC();
      OtpErlangObject indicator = result.elementAt(0);
      if ("ok".equals(indicator.toString())) {
        connection.sendRPC("erl_parse", "parse_exprs", new OtpErlangList(result.elementAt(1)));
        result = (OtpErlangTuple) connection.receiveRPC();
        indicator = result.elementAt(0);
        if ("ok".equals(indicator.toString())) {
          OtpErlangList forms = (OtpErlangList) result.elementAt(1);
          if (forms.arity() > 0) {
            connection.sendRPC("erl_eval", "exprs", new OtpErlangObject[]{ forms, bindings });
            result = (OtpErlangTuple) connection.receiveRPC();
            indicator = result.elementAt(0);
            if ("value".equals(indicator.toString())) {
              return result.elementAt(1);
            }
            else {
              OtpErlangObject errorInfo = result.elementAt(1);
              throw new MojoExecutionException("failed to evaluate form: " + errorInfo.toString());
            }
          }
          else {
            throw new MojoExecutionException("couldn't find forms to evaluate in expression");
          }
        }
        else {
          OtpErlangObject errorInfo = result.elementAt(1);
          throw new MojoExecutionException("failed to parse tokens: " + errorInfo.toString());
        }
      }
      else {
        OtpErlangObject errorInfo = result.elementAt(1);
        throw new MojoExecutionException("failed to scan expression: " + errorInfo.toString());
      }
    }
    catch (IOException e) {
      removeConnection(peer);
      throw new MojoExecutionException(e.getMessage(), e);
    }
    catch (OtpErlangExit e) {
      removeConnection(peer);
      throw new MojoExecutionException(e.getMessage(), e);
    }
    catch (OtpAuthException e) {
      removeConnection(peer);
      throw new MojoExecutionException(e.getMessage(), e);
    }
  }
}
