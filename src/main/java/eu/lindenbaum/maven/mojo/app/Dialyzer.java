package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.DialyzerScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MojoUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} runs the erlang {@code dialyzer} tool on the project
 * sources as well as the project includes. This means dialyzer will run over
 * the complete project code (excluding test modules).
 * </p>
 * <p>
 * The {@code dialyzer} can be skipped using the {@code skipDialyzer} parameter.
 * Additionally, the user can choose to run {@code dialyzer} also on the
 * projects dependencies using the {@code dialyzerWithDependencies} parameter.
 * This is disabled by default for the {@code erlang-otp} and {@code erlang-std}
 * project packaging.
 * </p>
 * 
 * @goal dialyzer
 * @phase prepare-package
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class Dialyzer extends ErlangMojo {
  /**
   * Setting this to {@code true} will skip the {@code dialyzer} analysis.
   * 
   * @parameter expression="${skipDialyzer}" default-value=false
   */
  private boolean skipDialyzer;

  /**
   * Setting this to {@code true} will include the projects dependencies into
   * the {@code dialyzer} run. Note: This may take very long.
   * 
   * @parameter expression="${dialyzerWithDependencies}" default-value=false
   */
  private boolean dialyzerWithDependencies;

  /**
   * Setting this to {@code true} will break the build when a {@code dialyzer}
   * run returns warnings.
   * 
   * @parameter expression="${dialyzerWarningsAreErrors}" default-value=false
   */
  private boolean dialyzerWarningsAreErrors;

  /**
   * Additional {@code dialyzer} warning options. This must be a comma separated
   * list with valid warning atoms that will be included when calling
   * <code>dialyzer:run([{warnings,[...]}, ...])</code>.
   * 
   * @parameter expression="${dialyzerOptions}"
   * @see http://www.erlang.org/doc/man/dialyzer.html
   */
  private String dialyzerOptions;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" D I A L Y Z E R");
    log.info(MavenUtils.SEPARATOR);

    if (this.skipDialyzer) {
      log.warn("Dialyzer is configured to be skipped.");
      return;
    }

    File lastBuildIndicator = new File(p.target(), ErlConstants.DIALYZER_OK);
    if (MojoUtils.newerFilesThan(p.src(), lastBuildIndicator)
        || MojoUtils.newerFilesThan(p.include(), lastBuildIndicator)
        || MojoUtils.newerFilesThan(p.targetLib(), lastBuildIndicator)) {
      lastBuildIndicator.delete();
      log.info("Running dialyzer on " + p.src());

      List<File> sources = new ArrayList<File>();
      sources.add(p.src());
      if (this.dialyzerWithDependencies) {
        sources.add(p.targetLib());
      }

      List<File> includes = MojoUtils.getIncludeDirectories(p);
      DialyzerScript script = new DialyzerScript(sources, includes, this.dialyzerOptions);
      String[] warnings = MavenSelf.get(p.cookie()).exec(p.node(), script, new ArrayList<File>());
      for (String warning : warnings) {
        log.warn(warning);
      }
      if (warnings.length > 0 && this.dialyzerWarningsAreErrors) {
        throw new MojoFailureException("Dialyzer emitted warnings.");
      }
      log.info("Dialyzer run successful.");

      try {
        if (warnings.length == 0) {
          lastBuildIndicator.createNewFile();
        }
      }
      catch (IOException e) {
        throw new MojoExecutionException("Failed to create " + lastBuildIndicator + ".");
      }
    }
    else {
      log.info("Last dialyzer run is still up to date.");
    }
  }
}
