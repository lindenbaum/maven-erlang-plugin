package eu.lindenbaum.maven.erlang;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that checks the integrity (correct term format) of an
 * application upgrade file.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class CheckAppUpScript extends AbstractScript<String> {
  private final File appUpFile;
  private final String version;

  /**
   * Creates a {@link Script} that checks a specific application upgrade file.
   * 
   * @param appUpFile the upgrade file to check
   * @param version the version of the application
   */
  public CheckAppUpScript(File appUpFile, String version) throws MojoExecutionException {
    super();
    this.appUpFile = appUpFile;
    this.version = version;
  }

  @Override
  public String get() {
    return String.format(this.script, this.appUpFile.getAbsolutePath(), this.version);
  }

  /**
   * Converts the result of the {@link Script} execution into an error string.
   * 
   * @param result to convert
   * @return An error string describing the error or {@code null} if none.
   */
  @Override
  public String handle(OtpErlangObject result) {
    if (result instanceof OtpErlangString) {
      return ((OtpErlangString) result).stringValue();
    }
    return null;
  }
}
