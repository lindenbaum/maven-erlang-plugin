package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} starting a list of erlang applications.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class StartApplicationScript implements Script<StartResult> {
  private static final String script = //
  "    CodePaths = %s," + //
      "Applications = %s," + //
      "code:add_pathsa(CodePaths)," + //
      "Before = [A || {A, _, _} <- application:which_applications()]," + //
      "Fun = fun([], ok, _) ->" + //
      "              ok;" + //
      "         ([A | Rest], ok, Rec) ->" + //
      "              case application:start(A) of" + //
      "                  ok ->" + //
      "                      Rec(Rest, ok, Rec);" + //
      "                  {error, already_started, _} ->" + //
      "                      Rec(Rest, ok, Rec);" + //
      "                  {error, {not_started, Dep}} ->" + //
      "                      Rec(Rest ++ [Dep, A], ok, Rec);" + //
      "                  Error ->" + //
      "                      Error" + //
      "              end;" + //
      "         (_, Error, _) ->" + //
      "              Error" + //
      "      end," + //
      "Result = Fun(Applications, ok, Fun)," + //
      "[code:del_path(P) || P <- CodePaths]," + //
      "{Result, Before}.";

  private final List<File> codePaths;
  private final List<String> applications;

  /**
   * Creates a {@link Script} trying to start a set of erlang applications.
   * 
   * @param codePaths needed to find the code
   * @param applications to start
   */
  public StartApplicationScript(List<File> codePaths, List<String> applications) {
    this.codePaths = codePaths;
    this.applications = applications;
  }

  @Override
  public String get() {
    String paths = ErlUtils.toFileList(this.codePaths, "\"", "\"");
    String applications = ErlUtils.toList(this.applications, null, "'", "'");
    return String.format(script, paths, applications);
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
    final OtpErlangObject sucess = resultTuple.elementAt(0);
    final OtpErlangList beforeList = (OtpErlangList) resultTuple.elementAt(1);
    return new StartResult() {
      @Override
      public boolean startSucceeded() {
        return "ok".equals(ErlUtils.cast(sucess));
      }

      @Override
      public void logError(Log log) {
        if (!startSucceeded()) {
          String[] lines = sucess.toString().split("\r?\n");
          for (String line : lines) {
            log.error(line);
          }
        }
      }

      @Override
      public List<String> getBeforeApplications() {
        ArrayList<String> resultList = new ArrayList<String>();
        for (int i = 0; i < beforeList.arity(); ++i) {
          resultList.add(ErlUtils.cast(beforeList.elementAt(i)));
        }
        return resultList;
      }
    };
  }
}
