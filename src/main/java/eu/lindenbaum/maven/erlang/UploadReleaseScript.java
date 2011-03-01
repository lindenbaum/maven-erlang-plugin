package eu.lindenbaum.maven.erlang;

import java.io.File;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangObject;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * A {@link Script} that uploads a release package into a specific nodes
 * {@code releases} directory.
 * </p>
 * <p>
 * Note: The target node process must have write access to its {@code releases}
 * directory in order to be able to execute this script.
 * </p>
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class UploadReleaseScript extends AbstractScript<GenericScriptResult> {
  private final String remoteNode;
  private final File releasePackage;

  public UploadReleaseScript(String remoteNode, File releasePackage) throws MojoExecutionException {
    super();
    this.remoteNode = remoteNode;
    this.releasePackage = releasePackage;
  }

  @Override
  public String get() {
    return String.format(this.script, this.remoteNode, this.releasePackage.getAbsolutePath());
  }

  /**
   * Converts the result of the {@link Script} execution into a generic result.
   * 
   * @return an instance of {@link GenericScriptResult}
   */
  @Override
  public GenericScriptResult handle(final OtpErlangObject result) {
    return new GenericScriptResult() {
      @Override
      public boolean success() {
        return "ok".equals(ErlUtils.toString(result));
      }

      @Override
      public void logOutput(Log log) {
        final LogLevel level;
        final String multiLineString;
        if (!success()) {
          level = LogLevel.ERROR;
          multiLineString = ErlUtils.toString(result);
          MavenUtils.logMultiLineString(log, level, multiLineString);
        }
      }
    };
  }
}
