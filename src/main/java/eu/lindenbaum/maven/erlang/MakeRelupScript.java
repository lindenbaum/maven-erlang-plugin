package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} creating a release upgrade file template using
 * <code>systools:make_relup/4</code>.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public final class MakeRelupScript extends AbstractScript<GenericScriptResult> {
  private final File relupFile;
  private final File relFile;
  private final List<String> releases;
  private final List<File> paths;

  /**
   * Creates a {@link Script} generating a release upgrade file.
   * 
   * @param relupFile the target relup template file to write
   * @param relFile the current release file to generate instructions for
   * @param releases a list of earlier versions to generate instructions for
   * @param paths a list of paths needed to find the relevant application files
   * @see <a
   *      href="http://www.erlang.org/doc/man/systools.html">http://www.erlang.org/doc/man/systools.html</a>
   */
  public MakeRelupScript(File relupFile, File relFile, List<String> releases, List<File> paths) throws MojoExecutionException {
    super();
    this.relupFile = relupFile;
    this.relFile = relFile;
    this.releases = releases;
    this.paths = paths;
  }

  @Override
  public String get() {
    String relup = this.relupFile.getAbsolutePath();
    String rel = this.relFile.getAbsolutePath().replace(ErlConstants.REL_SUFFIX, "");
    String releases = ErlUtils.toList(this.releases, null, "\"", "\"");
    String pathOption = "[{path, " + ErlUtils.toList(this.paths, null, "\"", "\"") + "}]";
    return String.format(this.script, relup, rel, releases, pathOption);
  }

  /**
   * Converts the result of the {@link Script} execution into an object capable
   * of logging the errors/warnings as well as returning the script result.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public GenericScriptResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    final String level = ErlUtils.toString(resultTuple.elementAt(0));
    final String messages = ErlUtils.toString(resultTuple.elementAt(1));
    return new GenericScriptResult() {
      @Override
      public boolean success() {
        return "ok".equals(level);
      }

      @Override
      public void logOutput(Log log) {
        if (!messages.isEmpty()) {
          MavenUtils.logMultiLineString(log, success() ? LogLevel.WARN : LogLevel.ERROR, messages);
        }
      }
    };
  }
}
