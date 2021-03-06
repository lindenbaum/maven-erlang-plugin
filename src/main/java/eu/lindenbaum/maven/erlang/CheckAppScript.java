package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that can be used to extract certain values from an erlang
 * application file.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class CheckAppScript extends AbstractScript<CheckAppResult> {
  private final File appFile;

  /**
   * Creates an extraction {@link Script} for a specific application file.
   * 
   * @param appFile to extract values from
   * @see <a
   *      href="http://www.erlang.org/doc/man/app.html">http://www.erlang.org/doc/man/app.html</a>
   */
  public CheckAppScript(File appFile) throws MojoExecutionException {
    super();
    this.appFile = appFile;
  }

  @Override
  public String get() {
    String appFilePath = this.appFile.getAbsolutePath();
    return String.format(this.script, appFilePath);
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
    final OtpErlangObject success = resultTuple.elementAt(0);
    final OtpErlangObject name = resultTuple.elementAt(1);
    final OtpErlangObject version = resultTuple.elementAt(2);
    final OtpErlangObject startModule = resultTuple.elementAt(3);

    final List<String> modules = new ArrayList<String>();
    OtpErlangList moduleList = (OtpErlangList) resultTuple.elementAt(4);
    for (int i = 0; i < moduleList.arity(); ++i) {
      modules.add(ErlUtils.toString(moduleList.elementAt(i)));
    }

    final List<String> applications = new ArrayList<String>();
    OtpErlangList applicationList = (OtpErlangList) resultTuple.elementAt(5);
    for (int i = 0; i < applicationList.arity(); ++i) {
      applications.add(ErlUtils.toString(applicationList.elementAt(i)));
    }

    return new CheckAppResult() {
      @Override
      public boolean success() {
        return "ok".equals(ErlUtils.toString(success));
      }

      @Override
      public String getVersion() {
        return ErlUtils.toString(version);
      }

      @Override
      public String getStartModule() {
        return ErlUtils.toString(startModule);
      }

      @Override
      public String getName() {
        return ErlUtils.toString(name);
      }

      @Override
      public List<String> getModules() {
        return modules;
      }

      @Override
      public List<String> getApplications() {
        return applications;
      }

      @Override
      public String toString() {
        return getName() + "::" + getVersion();
      }
    };
  }
}
