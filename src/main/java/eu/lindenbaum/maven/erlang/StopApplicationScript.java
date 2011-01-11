package eu.lindenbaum.maven.erlang;

import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} stopping all erlang applications except the ones given in a
 * preserver list.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class StopApplicationScript implements Script<Void> {
  private static final String script = //
  NL + "ToPreserve = %s, " + NL + //
      "ToStop = [A || {A, _, _} <- application:which_applications()] -- ToPreserve," + NL + //
      "[application:stop(A) || A <- ToStop]," + NL + //
      "[application:unload(A) || A <- ToStop]." + NL;

  private final List<String> applicationsToPreserve;

  /**
   * Creates a {@link Script} that stops all applications except a specific list
   * of applications.
   * 
   * @param applicationsToPreserve to not stop
   */
  public StopApplicationScript(List<String> applicationsToPreserve) {
    this.applicationsToPreserve = applicationsToPreserve;
  }

  @Override
  public String get() {
    String toPreserve = ErlUtils.toList(this.applicationsToPreserve, null, "'", "'");
    return String.format(script, toPreserve);
  }

  /**
   * The result of the {@link Script} execution is ignored.
   * 
   * @param the result term of the {@link Script} execution
   * @return Always {@code null}.
   */
  @Override
  public Void handle(OtpErlangObject result) {
    return null;
  }
}
