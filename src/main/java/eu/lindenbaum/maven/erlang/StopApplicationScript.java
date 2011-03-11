package eu.lindenbaum.maven.erlang;

import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangObject;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} stopping all erlang applications except the ones given in a
 * preserver list.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class StopApplicationScript extends AbstractScript<Void> {
  private final List<String> applicationsToPreserve;

  /**
   * Creates a {@link Script} that stops all applications except a specific list
   * of applications.
   * 
   * @param applicationsToPreserve to not stop
   */
  public StopApplicationScript(List<String> applicationsToPreserve) throws MojoExecutionException {
    super();
    this.applicationsToPreserve = applicationsToPreserve;
  }

  @Override
  public String get() {
    String toPreserve = ErlUtils.toList(this.applicationsToPreserve, null, "'", "'");
    return String.format(this.script, toPreserve);
  }

  /**
   * The result of the {@link Script} execution is ignored.
   * 
   * @param result the result term of the {@link Script} execution
   * @return Always {@code null}.
   */
  @Override
  public Void handle(OtpErlangObject result) {
    return null;
  }
}
