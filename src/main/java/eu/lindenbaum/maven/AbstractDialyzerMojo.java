package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.DIALYZER;
import static eu.lindenbaum.maven.util.ErlConstants.DIALYZER_OK;
import static eu.lindenbaum.maven.util.ErlUtils.exec;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import eu.lindenbaum.maven.util.Observer;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * Base class for {@link Mojo}s using the {@code dialyzer tool}.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Timo Koepke <timo.koepke@lindenbaum.eu>
 */
abstract class AbstractDialyzerMojo extends AbstractErlangMojo {
  /**
   * Setting this to {@code true} will force a {@code dialyzer} run even if this
   * would no be necessary because there are no new {@code .beam} files created
   * since the last run.
   * 
   * @parameter expression="${forceDialyzer}"
   */
  private boolean forceDialyzer;

  /**
   * Setting this to {@code true} will skip the {@code dialyzer} run even if the
   * {@link #forceDialyzer} variable is {@code true}.
   * 
   * @parameter expression="${skipDialyzer}"
   */
  private boolean skipDialyzer;

  /**
   * Additional {@code dialyzer} options.
   * 
   * @parameter
   * @see http://www.erlang.org/doc/man/dialyzer.html
   */
  private String[] dialyzerOptions;

  /**
   * Setting this to {@code true} will break the build when a {@code dialyzer}
   * run returns warnings.
   * 
   * @parameter default-value=false
   */
  boolean dialyzerWarningsAreErrors;

  /**
   * Starts a {@code dialyzer} run according to the project configuration on the
   * given directory with or without dependencies.
   * 
   * @param inputDirectory to run {@code dialyzer} on
   * @param withDependencies whether to include the project dependencies
   * @throws MojoFailureException
   * @throws MojoExecutionException
   */
  protected void execute(File inputDirectory, boolean withDependencies) throws MojoExecutionException,
                                                                       MojoFailureException {
    Log log = getLog();
    if (this.skipDialyzer) {
      log.info("Skipping dialyzer run.");
    }
    else {
      if (this.forceDialyzer || needDialyzerBuild(inputDirectory)) {
        List<File> libs = Collections.emptyList();
        if (withDependencies) {
          libs = getDependencies(this.targetLib);
        }
        dialyze(inputDirectory, libs);
      }
      else {
        log.info("Last dialyzer run is still up to date.");
      }
    }
  }

  /**
   * Returns whether a {@code dialyzer} run is currently necessary based on the
   * last runs timestamp.
   * 
   * @param dir input directory to check
   * @return true if there are .beam files newer than the last dialyzer run
   */
  private static boolean needDialyzerBuild(File dir) {
    final long lastDialyzerRunTime = new File(dir, DIALYZER_OK).lastModified();
    if (lastDialyzerRunTime > 0L) {
      File[] outdated = dir.listFiles(new FileFilter() {
        @Override
        public boolean accept(File entry) {
          String name = entry.getName();
          return name.endsWith(BEAM_SUFFIX) && entry.lastModified() > lastDialyzerRunTime;
        }
      });
      return outdated != null && outdated.length > 0;
    }
    return true;
  }

  /**
   * Runs the dialyzer on the given directory, additional directories can be
   * given with dependencies.
   * 
   * @param dir to dialyze
   * @param dependencies to dialyze
   */
  private void dialyze(File dir, List<File> dependencies) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    log.info("Running dialyzer on " + dir.getAbsolutePath());
    File lastBuildIndicator = new File(dir, DIALYZER_OK);
    lastBuildIndicator.delete();

    List<String> command = new ArrayList<String>();
    command.add(DIALYZER);
    command.add("-r");
    command.add(dir.getAbsolutePath());
    for (File dependency : dependencies) {
      command.add(dependency.getAbsolutePath());
    }
    if (this.dialyzerOptions != null) {
      command.addAll(Arrays.asList(this.dialyzerOptions));
    }
    exec(command, log, dir, new Observer() {
      @Override
      public String handle(int exitValue, String result) throws MojoExecutionException, MojoFailureException {
        if (exitValue == 2 && AbstractDialyzerMojo.this.dialyzerWarningsAreErrors) {
          throw new MojoFailureException("Dialyzer emitted warnings.");
        }
        else if (exitValue == 1) {
          throw new MojoFailureException("Dialyzer found errors.");
        }
        else if (exitValue != 0) {
          throw new MojoExecutionException("Dialyzer returned with " + exitValue);
        }
        return null;
      }
    });
    try {
      lastBuildIndicator.createNewFile();
    }
    catch (IOException e) {
      throw new MojoExecutionException("Failed to create " + lastBuildIndicator.getAbsolutePath());
    }
  }
}
