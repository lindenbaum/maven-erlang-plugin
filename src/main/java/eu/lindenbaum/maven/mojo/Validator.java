package eu.lindenbaum.maven.mojo;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;

import com.ericsson.otp.erlang.OtpErlangAtom;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;

/**
 * {@link Mojo} that checks the project configuration for legal values.
 * 
 * @goal validate
 * @phase validate
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public final class Validator extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    MavenProject project = p.project();

    String applicationName = new OtpErlangAtom(project.getArtifactId()).toString();
    if (applicationName.startsWith("'")) {
      log.error("The name " + applicationName + " is not a valid project artifact id.");
      log.error("Artifact id must be an unquoted erlang atom, e.g. no dashes.");
      throw new MojoExecutionException("Invalid artifact id " + applicationName + ".");
    }
  }
}
