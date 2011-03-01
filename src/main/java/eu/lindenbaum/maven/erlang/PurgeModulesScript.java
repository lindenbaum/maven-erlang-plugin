package eu.lindenbaum.maven.erlang;

import com.ericsson.otp.erlang.OtpErlangObject;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that purges all modules currently loaded except the ones
 * loaded directly from the backends lib directory retrieved using
 * <code>code:lib_dir/0</code> or belonging to the <code>Emacs/distel</code>
 * project.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class PurgeModulesScript extends AbstractScript<Void> {
  public PurgeModulesScript() throws MojoExecutionException {
    super();
  }

  @Override
  public String get() {
    return this.script;
  }

  /**
   * The result of the {@link Script} execution is ignored.
   */
  @Override
  public Void handle(OtpErlangObject result) {
    return null;
  }
}
