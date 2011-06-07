package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.DialyzerScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;
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

    File lastBuildIndicator = p.targetLayout().dialyzerOk();
    List<File> directories = new ArrayList<File>();
    directories.add(p.sourceLayout().src());
    directories.add(p.sourceLayout().include());
    directories.add(p.targetLayout().lib());

    if (MojoUtils.newerFilesThan(lastBuildIndicator, directories)) {
      FileUtils.removeFiles(lastBuildIndicator);

      List<File> sources = new ArrayList<File>();
      sources.add(p.sourceLayout().src());
      if (this.dialyzerWithDependencies) {
        sources.add(p.targetLayout().lib());
      }

      DialyzerScript script = new DialyzerScript(sources, p.includePaths(), this.dialyzerOptions);
      String[] output = MavenSelf.get(p.cookie()).exec(p.node(), script);

      List<File> files = FileUtils.getFilesRecursive(p.sourceLayout().src(), ErlConstants.ERL_SUFFIX);
      files.addAll(FileUtils.getFilesRecursive(p.targetLayout().lib(), ErlConstants.ERL_SUFFIX));
      Collection<String> warnings = MojoUtils.parseDialyzerOutput(output, files);
      if (warnings.size() > 0) {
        log.warn("Warnings:");
        MavenUtils.logCollection(log, LogLevel.WARN, warnings, "");
        if (this.dialyzerWarningsAreErrors) {
          throw new MojoFailureException("Dialyzer reported warnings.");
        }
      }
      else {
        log.info("Dialyzer run successful.");
        FileUtils.touch(lastBuildIndicator);
      }
    }
    else {
      log.info("Last dialyzer run is still up to date.");
    }
  }
}
