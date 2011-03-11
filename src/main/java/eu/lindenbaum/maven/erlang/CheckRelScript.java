package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that can be used to extract certain values from an erlang
 * release file.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class CheckRelScript extends AbstractScript<CheckRelResult> {
  private final File relFile;

  /**
   * Creates an extraction {@link Script} for a specific release file.
   * 
   * @param relFile to extract values from
   * @see <a
   *      href="http://www.erlang.org/doc/man/rel.html">http://www.erlang.org/doc/man/rel.html</a>
   */
  public CheckRelScript(File relFile) throws MojoExecutionException {
    super();
    this.relFile = relFile;
  }

  @Override
  public String get() {
    return String.format(this.script, this.relFile.getAbsolutePath());
  }

  /**
   * Converts the result of the {@link Script} execution into a
   * {@link CheckRelResult} bean holding interesting values from the release
   * file.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public CheckRelResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    final OtpErlangObject success = resultTuple.elementAt(0);
    final OtpErlangObject releaseName = resultTuple.elementAt(1);
    final OtpErlangObject releaseVersion = resultTuple.elementAt(2);
    final OtpErlangObject ertsVersion = resultTuple.elementAt(3);

    OtpErlangList applicationList = (OtpErlangList) resultTuple.elementAt(4);
    final Map<String, String> applications = new HashMap<String, String>();
    for (int i = 0; i < applicationList.arity(); ++i) {
      OtpErlangTuple appTuple = (OtpErlangTuple) applicationList.elementAt(i);
      String application = ErlUtils.toString(appTuple.elementAt(0));
      String version = ErlUtils.toString(appTuple.elementAt(1));
      applications.put(application, version);
    }

    return new CheckRelResult() {
      @Override
      public boolean success() {
        return "ok".equals(ErlUtils.toString(success));
      }

      @Override
      public String getName() {
        return ErlUtils.toString(releaseName);
      }

      @Override
      public String getReleaseVersion() {
        return ErlUtils.toString(releaseVersion);
      }

      @Override
      public String getErtsVersion() {
        return ErlUtils.toString(ertsVersion);
      }

      @Override
      public Map<String, String> getApplications() {
        return applications;
      }

      @Override
      public String toString() {
        return getName() + "::" + getReleaseVersion();
      }
    };
  }
}
