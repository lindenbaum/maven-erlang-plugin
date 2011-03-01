package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} that uploads a list of compiled modules and application
 * files onto a specific (remote) node. Modules will be purged ones before
 * loaded, application files will not be written but loaded directly using
 * <code>application:load/1</code>.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class UploadScript extends AbstractScript<GenericScriptResult> {
  private final String remoteNode;
  private final List<File> beamFiles;
  private final List<File> appFiles;

  public UploadScript(String remoteNode, List<File> beamFiles, List<File> appFiles) throws MojoExecutionException {
    super();
    this.remoteNode = remoteNode;
    this.beamFiles = beamFiles;
    this.appFiles = appFiles;
  }

  @Override
  public String get() {
    String beamFileList = ErlUtils.toFileList(this.beamFiles, "\"", "\"");
    String appFileList = ErlUtils.toFileList(this.appFiles, "\"", "\"");
    return String.format(this.script, this.remoteNode, beamFileList, appFileList);
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
        return result instanceof OtpErlangList;
      }

      @Override
      public void logOutput(Log log) {
        final LogLevel level;
        final String multiLineString;
        if (!success()) {
          level = LogLevel.ERROR;
          multiLineString = ErlUtils.toString(result);
        }
        else {
          level = LogLevel.INFO;
          multiLineString = ErlUtils.toString(result).replace(",", "," + NL);
        }
        MavenUtils.logMultiLineString(log, level, multiLineString);
      }
    };
  }
}
