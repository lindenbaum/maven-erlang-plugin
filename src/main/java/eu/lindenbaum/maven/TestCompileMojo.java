package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.ERL_SUFFIX;
import static eu.lindenbaum.maven.util.FileUtils.removeFilesRecursive;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * Compile erlang test sources and recompile erlang sources with debug
 * information using the {@code export_all} option.
 * 
 * @goal test-compile
 * @phase test-compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestCompileMojo extends AbstractCompilerMojo {
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

    this.targetTest.mkdirs();
    int removed = removeFilesRecursive(this.targetTest, BEAM_SUFFIX);
    log.info("Removed " + removed + " stale " + BEAM_SUFFIX + "-files from " + this.targetTest);

    int i = 0;
    if (this.srcMainErlang.exists()) {
      i += compile(this.srcMainErlang, this.targetTest, null, ERL_SUFFIX, BEAM_SUFFIX, options);
    }
    if (this.srcTestErlang.exists()) {
      i += compile(this.srcTestErlang, this.targetTest, this.srcTestInclude, ERL_SUFFIX, BEAM_SUFFIX, options);
    }
    if (i == 0) {
      log.info("No sources to compile");
    }
    else {
      log.info("Compiled " + i + " files");
    }
  }
}
