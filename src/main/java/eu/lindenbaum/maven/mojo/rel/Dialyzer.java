package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.DialyzerScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MojoUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} runs the erlang {@code dialyzer} tool on the release
 * dependencies. The {@code dialyzer} can be skipped using the
 * {@code skipDialyzer} parameter.
 * </p>
 * 
 * @goal dialyzer-release
 * @phase compile
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
    if (MojoUtils.newerFilesThan(p.targetLib(), lastBuildIndicator)) {
      FileUtils.removeFiles(lastBuildIndicator);
      log.info("Running dialyzer on " + p.targetLib());

      List<File> sources = Arrays.asList(new File[]{ p.targetLib() });
      List<File> includes = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.HRL_SUFFIX);

      String[] dependenciesToAnalyze = p.targetLib().list();
      if (dependenciesToAnalyze == null || dependenciesToAnalyze.length == 0) {
        if (this.dialyzerWarningsAreErrors) {
          throw new MojoFailureException("No sources to analyze.");
        }
        else {
          log.warn("No sources to analyze.");
        }
        return;
      }

      DialyzerScript script = new DialyzerScript(sources, includes, this.dialyzerOptions);
      String[] warnings = MavenSelf.get(p.cookie()).exec(p.node(), script);
      for (String warning : warnings) {
        log.warn(warning);
      }
      if (warnings.length > 0 && this.dialyzerWarningsAreErrors) {
        throw new MojoFailureException("Dialyzer emitted warnings.");
      }
      log.info("Dialyzer run successful.");
      FileUtils.touch(lastBuildIndicator);
    }
    else {
      log.info("Last dialyzer run is still up to date.");
    }
  }
}
