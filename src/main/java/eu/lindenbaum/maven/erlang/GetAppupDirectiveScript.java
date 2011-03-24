package eu.lindenbaum.maven.erlang;

import java.io.File;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that returns the appup directive for a certain module. It is
 * assumed that the module is neither newly introduced nor deleted in the
 * update.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public class GetAppupDirectiveScript extends AbstractScript<String> {
  private final String module;
  private final File modulePath;

  /**
   * Returns the appup directive for a certain module.
   * 
   * @param module to generate the directive for
   * @param modulePath path to the parent directory of the module's beam file
   */
  public GetAppupDirectiveScript(String module, File modulePath) throws MojoExecutionException {
    super();
    this.module = module;
    this.modulePath = modulePath;
  }

  @Override
  public String get() {
    return String.format(this.script, this.module, this.modulePath.getAbsolutePath());
  }

  /**
   * Converts the result of the {@link Script} execution into a {@link String}
   * containing either "error" or the appup directive for the module.
   * 
   * @param result to convert
   * @return A {@link String} object, never {@code null}
   */
  @Override
  public String handle(OtpErlangObject result) {
    if (result instanceof OtpErlangAtom) {
      return null;
    }
    return ErlUtils.toString(result);
  }
}
