package eu.lindenbaum.maven.util;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Containing utilities related to the erlang dialyzer.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Timo Koepke <timo.koepke@lindenbaum.eu>
 */
public final class DialyzerUtils {
  /**
   * Returns whether a dialyzer run is currently necessary based on the last runs timestamp.
   * 
   * @param dir to dialyze
   * @return true if there are .beam files newer than the last dialyzer run
   */
  public static boolean needDialyzerBuild(File dir) {
    File lastBuildIndicator = new File(dir, ErlConstants.DIALYZER_OK);
    if (lastBuildIndicator.exists()) {
      File[] beams = dir.listFiles(new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
          return name.endsWith(ErlConstants.BEAM_SUFFIX);
        }
      });

      long lastDialyzerRunTime = lastBuildIndicator.lastModified();
      for (File beam : beams) {
        if (beam.lastModified() > lastDialyzerRunTime) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Runs the dialyzer on the given directory, additional directories can be given with dependencies.
   * 
   * @param log logger to use
   * @param dir to dialyze
   * @param dependencies to dialyze
   * @param options optional dialyzer parameters, maybe {@code null}
   * @param warningsAreErrors indicating whether dialyzer warnings break the build
   * @throws MojoExecutionException
   */
  public static void dialyze(final Log log,
                             File dir,
                             List<File> dependencies,
                             String[] options,
                             final boolean warningsAreErrors) throws MojoExecutionException {
    File lastBuildIndicator = new File(dir, ErlConstants.DIALYZER_OK);
    lastBuildIndicator.delete();

    log.info("Running dialyzer on " + dir.getAbsolutePath());
    List<String> command = new ArrayList<String>();
    command.add(ErlConstants.DIALYZER);
    command.add("-r");
    command.add(dir.getAbsolutePath());
    for (File dependency : dependencies) {
      command.add(dependency.getAbsolutePath());
    }
    if (options != null) {
      command.addAll(Arrays.asList(options));
    }
    ErlUtils.exec(command, log, dir, new ProcessListener() {
      @Override
      public String processCompleted(int exitValue, List<String> processOutput) throws MojoExecutionException {
        for (String line : processOutput) {
          if (warningsAreErrors) {
            log.error(line);
          }
          else {
            log.warn(line);
          }
        }
        if (exitValue != 0) {
          if (exitValue == 2 && warningsAreErrors) {
            throw new MojoExecutionException("Warnings with dialyzer (are errors)");
          }
          else if (exitValue == 1) {
            throw new MojoExecutionException("Errors with dialyzer");
          }
          else if (exitValue != 2) {
            throw new MojoExecutionException("Unknown dialyzer return code (" + exitValue + ")");
          }
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
