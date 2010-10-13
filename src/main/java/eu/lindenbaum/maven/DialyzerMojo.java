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
 * This {@link Mojo} runs the erlang {@code dialyzer} tool on the project
 * sources found in {@link AbstractErlangMojo#srcMainErlang} as well as the
 * project includes in {@link AbstractErlangMojo#srcMainInclude}. This means
 * dialyzer will run over the complete project code (excluding test modules).
 * </p>
 * <p>
 * The {@code dialyzer} can be skipped using the {@code useDialyzer} parameter
 * in the projects pom. Additionally, the user can choose to run
 * {@code dialyzer} also on the projects dependencies using the
 * {@code dialyzerWithDependencies} pom parameter. This is disabled by default
 * for the {@code erlang-otp} application packaging.
 * </p>
 * 
 * @goal dialyzer
 * @phase process-test-classes
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class DialyzerMojo extends AbstractDialyzerMojo {
  /**
   * Setting this to {@code true} will skip the {@code dialyzer} analysis.
   * 
   * @parameter default-value=false
   */
  private boolean skipDialyzer;

  /**
   * Setting this to {@code true} will include the projects dependencies into
   * the {@code dialyzer} run. Note: This may take very long.
   * 
   * @parameter default-value=false
   */
  private boolean dialyzerWithDependencies;

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
      boolean newerSources = newerFilesThan(this.srcMainErlang, lastBuildIndicator);
      boolean newerHeaders = newerFilesThan(this.srcMainInclude, lastBuildIndicator);
      if (newerSources || newerHeaders) {
        lastBuildIndicator.delete();
        log.info("Running dialyzer on " + this.srcMainErlang.getAbsolutePath());

        List<String> command = new ArrayList<String>();
        command.add(DIALYZER);
        command.add("--src");
        command.add("-r");
        command.add(this.srcMainErlang.getPath());
        if (this.srcMainInclude.isDirectory()) {
          command.add("-I");
          command.add(this.srcMainInclude.getPath());
        }
        if (this.dialyzerWithDependencies) {
          command.add("-r");
          command.add(this.targetLib.getPath());
          for (File include : getDependencyIncludes(this.targetLib)) {
            command.add("-I");
            command.add(include.getPath());
          }
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
