package eu.lindenbaum.maven;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.ProcessListener;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Abstract base class for {@link Mojo}s related to the erlang dialyzer.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
abstract class AbstractDialyzerMojo extends AbstractErlMojo {
  /**
   * Name of the dialyzer ok file.
   */
  private static final String DIALYZER_OK = ".dialyzer.ok";

  /**
   * Additional dialyzer options.
   * 
   * @parameter
   */
  private String[] dialyzerOptions;

  /**
   * If dialyzer warnings are to be considered as errors.
   * 
   * @parameter default-value=false
   */
  private boolean dialyzerWarningsAreErrors;

  /**
   * If dialyzer should be run with the dependencies. This is *much* slower.
   * 
   * @parameter default-value=false
   */
  private boolean dialyzerWithDependencies;

  /**
   * Set this to 'true' to force running dialyzer, i.e. even if beams are not newer than latest run.
   * 
   * @parameter expression="${forceDialyzer}"
   */
  private boolean forceDialyzer;

  /**
   * Set this to 'true' to skip dialyzer.
   * 
   * @parameter expression="${skipDialyzer}"
   */
  protected boolean skipDialyzer;

  /**
   * Set this to 'true' to skip running tests. This also skips dialyzer.
   * 
   * @parameter expression="${maven.test.skip}"
   */
  protected boolean maven_test_skip;

  /**
   * Set the dialyzerWithDependencies option.
   * 
   * @param inValue value for the option
   */
  protected final void setDialyzerWithDependencies(boolean inValue) {
    this.dialyzerWithDependencies = inValue;
  }

  /**
   * Run dialyzer on the generated binaries, unless the binaries are older than the dialyzer.ok file.
   * 
   * @param outputDirectory directory where .beam files are located.
   * @throws MojoExecutionException if there is a problem with the execution of dialyzer or if dialyzer
   *           detected problems with the code.
   */
  protected final void dialyzer(final File outputDirectory) throws MojoExecutionException {
    if (!outputDirectory.exists()) {
      outputDirectory.mkdirs();
    }

    final File dialyzerOkFile = new File(outputDirectory, DIALYZER_OK);
    boolean skipDialyzer = false;
    if (!this.forceDialyzer && dialyzerOkFile.exists()) {
      skipDialyzer = true;
      long dialyzerOkFileDate = dialyzerOkFile.lastModified();
      for (File theOutputFile : outputDirectory.listFiles()) {
        if (theOutputFile.getName().endsWith(ErlConstants.BEAM_SUFFIX)) {
          if (theOutputFile.lastModified() > dialyzerOkFileDate) {
            skipDialyzer = false;
            break;
          }
        }
      }
    }

    final Log log = getLog();
    if (skipDialyzer) {
      log.debug("Skipping dialyzer since beam files are older than last successful run.");
    }
    else {
      dialyzerOkFile.delete();
      log.info("Running dialyzer in " + outputDirectory.getPath());

      // Generate the command line.
      List<String> command = new LinkedList<String>();
      command.add(ErlConstants.DIALYZER);
      command.add("-r");
      command.add(outputDirectory.getPath());
      if (this.dialyzerWithDependencies) {
        command.addAll(getLibPaths());
      }
      if (this.dialyzerOptions != null) {
        command.addAll(Arrays.asList(this.dialyzerOptions));
      }
      final boolean dialyzerWarningsAreErrors = this.dialyzerWarningsAreErrors;
      ErlUtils.exec(command, log, outputDirectory, new ProcessListener() {
        @Override
        public String processCompleted(int exitValue, List<String> processOutput) throws MojoExecutionException {
          for (String line : processOutput) {
            if (dialyzerWarningsAreErrors) {
              log.error(line);
            }
            else {
              log.warn(line);
            }
          }
          if (exitValue != 0) {
            if (exitValue == 2 && dialyzerWarningsAreErrors) {
              throw new MojoExecutionException("Warnings with dialyzer (are errors)");
            }
            else if (exitValue == 1) {
              throw new MojoExecutionException("Errors with dialyzer");
            }
            else if (exitValue != 2) {
              throw new MojoExecutionException("Unknown dialyzer return code (" + exitValue + ")");
            }
          }
          try {
            dialyzerOkFile.createNewFile();
          }
          catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
          }
          return null;
        }
      });
    }
  }
}
