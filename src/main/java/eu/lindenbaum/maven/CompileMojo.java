package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.BIN_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.ERL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.HRL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.MIB_SUFFIX;
import static eu.lindenbaum.maven.util.FileUtils.removeFilesRecursive;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * Compile erlang sources.
 * 
 * @goal compile
 * @phase compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class CompileMojo extends AbstractCompilerMojo {
  /**
   * Directory where the erlang source files reside.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   */
  private File srcMainErlang;

  /**
   * Directory where the SNMP specific files get copied by the {@link CopyResourcesMojo}.
   * 
   * @parameter expression="${project.build.directory}/mibs/"
   */
  private File mibsInput;

  /**
   * Directory where the SNMP specific output is to be placed.
   * 
   * @parameter expression="${project.build.directory}/priv/mibs"
   */
  private File mibsOutput;

  /**
   * Directory where the compiled sources will be placed into.
   * 
   * @parameter expression="${project.build.directory}/ebin/"
   */
  private File ebinOutput;

  /**
   * Setting this to {@code true} will compile all modules with debug information.
   * 
   * @parameter default-value=false
   */
  private boolean debugInfo;

  /**
   * Additional compiler options.
   * 
   * @parameter
   */
  private String[] erlcOptions;

  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();

    List<String> options = new ArrayList<String>();
    if (this.erlcOptions != null) {
      options.addAll(Arrays.asList(this.erlcOptions));
    }
    if (this.debugInfo) {
      options.add("+debug_info");
    }

    int compiled = 0;
    if (this.mibsInput.exists()) {
      this.mibsOutput.mkdirs();
      int removed = removeFilesRecursive(this.mibsOutput, BIN_SUFFIX);
      log.info("Removed " + removed + " stale " + BIN_SUFFIX + "-files from " + this.mibsOutput);
      compiled += compile(this.mibsInput, this.mibsOutput, null, MIB_SUFFIX, BIN_SUFFIX, options);
    }
    if (this.mibsOutput.exists()) {
      this.includeOutput.mkdirs();
      int removed = removeFilesRecursive(this.includeOutput, HRL_SUFFIX);
      log.info("Removed " + removed + " stale " + HRL_SUFFIX + "-files from " + this.includeOutput);
      compiled += compile(this.mibsOutput, this.includeOutput, null, BIN_SUFFIX, HRL_SUFFIX, options);
    }
    if (this.srcMainErlang.exists()) {
      this.ebinOutput.mkdirs();
      int removed = removeFilesRecursive(this.ebinOutput, BEAM_SUFFIX);
      log.info("Removed " + removed + " stale " + BEAM_SUFFIX + "-files from " + this.ebinOutput);
      compiled += compile(this.srcMainErlang, this.ebinOutput, null, ERL_SUFFIX, BEAM_SUFFIX, options);
    }
    if (compiled == 0) {
      log.info("No sources to compile");
    }
    else {
      log.info("Compiled " + compiled + " files");
    }
  }
}
