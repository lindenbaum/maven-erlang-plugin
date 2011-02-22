package eu.lindenbaum.maven.erlang;

import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} starting a list of erlang applications.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class StartApplicationScript implements Script<StartResult> {
  private static final String script = //
  NL + "Applications = %s," + NL + //
      "Before = [A || {A, _, _} <- application:which_applications()]," + NL + //
      "Fun = fun([], ok, _) ->" + NL + //
      "              ok;" + NL + //
      "         ([A | Rest], ok, Rec) ->" + NL + //
      "              case application:start(A) of" + NL + //
      "                  ok ->" + NL + //
      "                      Rec(Rest, ok, Rec);" + NL + //
      "                  {error, {already_started, _}} ->" + NL + //
      "                      Rec(Rest, ok, Rec);" + NL + //
      "                  {error, {not_started, Dep}} ->" + NL + //
      "                      Rec([Dep, A] ++ Rest, ok, Rec);" + NL + //
      "                  Error ->" + NL + //
      "                      Error" + NL + //
      "              end;" + NL + //
      "         (_, Error, _) ->" + NL + //
      "              Error" + NL + //
      "      end," + NL + //
      "Result = Fun(Applications, ok, Fun)," + NL + //
      "{Result, Before}." + NL;

  private final List<String> applications;

  /**
   * Creates a {@link Script} trying to start a set of erlang applications.
   * 
   * @param codePaths needed to find the code
   * @param applications to start
   */
  public StartApplicationScript(List<String> applications) {
    this.applications = applications;
  }

  @Override
  public String get() {
    String applications = ErlUtils.toList(this.applications, null, "'", "'");
    return String.format(script, applications);
  }

  /**
   * Converts the result of the {@link Script} execution into an object capable
   * of logging the test output as well as returning whether the unit test
   * execution succeeded.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public StartResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    final String resultMessage = ErlUtils.toString(resultTuple.elementAt(0));
    final OtpErlangList beforeList = (OtpErlangList) resultTuple.elementAt(1);
    return new StartResult() {
      @Override
      public boolean startSucceeded() {
        return "ok".equals(resultMessage);
      }

      @Override
      public void logError(Log log) {
        if (!startSucceeded()) {
          MavenUtils.logMultiLineString(log, LogLevel.ERROR, resultMessage);
        }
      }

      @Override
      public List<String> getBeforeApplications() {
        ArrayList<String> resultList = new ArrayList<String>();
        for (int i = 0; i < beforeList.arity(); ++i) {
          resultList.add(ErlUtils.toString(beforeList.elementAt(i)));
        }
        return resultList;
      }
    };
  }
}
