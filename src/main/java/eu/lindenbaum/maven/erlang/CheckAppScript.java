package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} that can be used to extract certain values from an erlang
 * application file.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class CheckAppScript implements Script<CheckAppResult> {
  private static final String script = //
  NL + "case file:consult(\"%s\") of" + NL + //
      "    {ok, [{application, A, Props}]} ->" + NL + //
      "        V = proplists:get_value(vsn, Props, undefined)," + NL + //
      "        S = proplists:get_value(mod, Props, omitted)," + NL + //
      "        M = proplists:get_value(modules, Props, [])," + NL + //
      "        D = proplists:get_value(applications, Props, [])," + NL + //
      "        case S of {Module, _} -> {A, V, Module, M, D}; _ -> {A, V, S, M, D} end;" + NL + //
      "    _ ->" + NL + //
      "        {undefined, undefined, undefined, [], []}" + NL + //
      "end." + NL;

  private final File appFile;

  /**
   * Creates an extraction {@link Script} for a specific application file.
   * 
   * @param appFile to extract values from
   * @see http://www.erlang.org/doc/man/app.html
   */
  public CheckAppScript(File appFile) {
    this.appFile = appFile;
  }

  @Override
  public String get() {
    String appFilePath = this.appFile.getAbsolutePath();
    return String.format(script, appFilePath);
  }

  /**
   * Converts the result of the {@link Script} execution into a
   * {@link CheckAppResult} bean holding interesting values from the application
   * file.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public CheckAppResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    final OtpErlangObject name = resultTuple.elementAt(0);
    final OtpErlangObject version = resultTuple.elementAt(1);
    final OtpErlangObject startModule = resultTuple.elementAt(2);
    final OtpErlangObject modules = resultTuple.elementAt(3);
    final OtpErlangObject applications = resultTuple.elementAt(4);
    return new CheckAppResult() {
      @Override
      public String getVersion() {
        return ErlUtils.cast(version);
      }

      @Override
      public String getStartModule() {
        return ErlUtils.cast(startModule);
      }

      @Override
      public String getName() {
        return ErlUtils.cast(name);
      }

      @Override
      public List<String> getModules() {
        List<String> r = new ArrayList<String>();
        OtpErlangList m = (OtpErlangList) modules;
        for (int i = 0; i < m.arity(); ++i) {
          r.add(ErlUtils.cast(m.elementAt(i)));
        }
        return r;
      }

      @Override
      public List<String> getApplications() {
        List<String> r = new ArrayList<String>();
        OtpErlangList a = (OtpErlangList) applications;
        for (int i = 0; i < a.arity(); ++i) {
          r.add(ErlUtils.cast(a.elementAt(i)));
        }
        return r;
      }
    };
  }
}
