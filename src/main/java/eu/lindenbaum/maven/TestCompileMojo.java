package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.ERL_SUFFIX;
import static eu.lindenbaum.maven.util.FileUtils.removeFilesRecursive;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * Compile erlang test sources and recompile erlang sources using the {@code export_all} option.
 * 
 * @goal test-compile
 * @phase test-compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestCompileMojo extends AbstractCompilerMojo {
  /**
   * Directory where the erlang source files reside.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   * @required
   */
  private File srcMainErlang;

  /**
   * Directory where the erlang test source files reside.
   * 
   * @parameter expression="${basedir}/src/test/erlang/"
   * @required
   */
  private File srcTestErlang;

  /**
   * Directory where the erlang test include files reside.
   * 
   * @parameter expression="${basedir}/src/test/include"
   */
  private File include;

  /**
   * Directory where the compiled test sources and recompiled sources will be placed into.
   * 
   * @parameter expression="${project.build.directory}/test"
   * @required
   */
  private File testOutput;

  /**
   * Additional compiler options for test compilation.
   * 
   * @parameter
   */
  private String[] erlcTestOptions;

  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();

    List<String> options = new ArrayList<String>();
    if (this.erlcTestOptions != null) {
      options.addAll(Arrays.asList(this.erlcTestOptions));
    }
    options.add("+debug_info");
    options.add("+export_all");

    this.testOutput.mkdirs();
    int removed = removeFilesRecursive(this.testOutput, BEAM_SUFFIX);
    log.info("Removed " + removed + " stale " + BEAM_SUFFIX + "-files from " + this.testOutput);

    int compiled = 0;
    if (this.srcMainErlang.exists()) {
      compiled += compile(this.srcMainErlang, this.testOutput, null, ERL_SUFFIX, BEAM_SUFFIX, options);
    }
    if (this.srcTestErlang.exists()) {
      compiled += compile(this.srcTestErlang, this.testOutput, this.include, ERL_SUFFIX, BEAM_SUFFIX, options);
    }
    if (compiled == 0) {
      log.info("No sources to compile");
    }
    else {
      log.info("Compiled " + compiled + " files");
    }
  }
}
