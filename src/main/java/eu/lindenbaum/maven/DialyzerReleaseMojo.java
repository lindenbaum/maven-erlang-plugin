package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.DIALYZER;
import static eu.lindenbaum.maven.util.ErlConstants.DIALYZER_OK;
import static eu.lindenbaum.maven.util.FileUtils.getDependencyIncludes;
import static eu.lindenbaum.maven.util.FileUtils.newerFilesThan;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} runs the erlang {@code dialyzer} tool on a complete release
 * found in {@link AbstractErlangMojo#targetLib}. The {@code dialyzer} can be
 * skipped using the {@code useDialyzer} paramter in the projects pom. Since
 * this {@link Mojo} is called in order to check a complete release it is run
 * over all release dependencies.
 * </p>
 * 
 * @goal dialyzer-release
 * @phase compile
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class DialyzerReleaseMojo extends AbstractDialyzerMojo {
  /**
   * Setting this to {@code true} will skip the {@code dialyzer} analysis.
   * 
   * @parameter default-value=false
   */
  private boolean skipDialyzer;

  /**
   * Additional {@code dialyzer} options.
   * 
   * @parameter
   * @see http://www.erlang.org/doc/man/dialyzer.html
   */
  private String[] dialyzerOptions;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    if (this.skipDialyzer) {
      log.warn("Dialyzer is configured to be skipped.");
    }
    else {
      File lastBuildIndicator = new File(DIALYZER_OK);
      if (newerFilesThan(this.targetLib, lastBuildIndicator)) {
        lastBuildIndicator.delete();
        log.info("Running dialyzer on " + this.targetLib.getAbsolutePath());

        List<String> command = new ArrayList<String>();
        command.add(DIALYZER);
        command.add("--src");
        command.add("-r");
        command.add(this.targetLib.getPath());
        for (File include : getDependencyIncludes(this.targetLib)) {
          command.add("-I");
          command.add(include.getPath());
        }
        if (this.dialyzerOptions != null) {
          command.addAll(Arrays.asList(this.dialyzerOptions));
        }
        dialyze(command);
        try {
          lastBuildIndicator.createNewFile();
        }
        catch (IOException e) {
          throw new MojoExecutionException("Failed to create " + lastBuildIndicator.getAbsolutePath());
        }
      }
      else {
        log.info("Last dialyzer run is still up to date.");
      }
    }
  }
}
