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

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * This {@link Mojo} compiles the projects erlang sources and SNMP files.
 * 
 * @goal compile
 * @phase compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class CompileMojo extends AbstractCompilerMojo {
  /**
   * Setting this to {@code true} will compile all modules with debug
   * information.
   * 
   * @parameter expression=${debugInfo} default-value=false
   */
  private boolean debugInfo;

  /**
   * Additional compiler options.
   * 
   * @parameter
   */
  private String[] erlcOptions;

  @Override
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
    File inc = this.srcMainErlang; // "private" includes may be used
    if (this.srcMainErlang.exists()) {
      this.targetMibs.mkdirs();
      int removed = removeFilesRecursive(this.targetMibs, BIN_SUFFIX);
      log.info("Removed " + removed + " stale " + BIN_SUFFIX + "-files from " + this.targetMibs);
      compiled += compile(this.srcMainErlang, this.targetMibs, inc, MIB_SUFFIX, BIN_SUFFIX, options);
    }
    if (this.targetMibs.exists()) {
      this.targetInclude.mkdirs();
      int removed = removeFilesRecursive(this.targetInclude, HRL_SUFFIX);
      log.info("Removed " + removed + " stale " + HRL_SUFFIX + "-files from " + this.targetInclude);
      compiled += compile(this.targetMibs, this.targetInclude, inc, BIN_SUFFIX, HRL_SUFFIX, options);
    }
    if (this.srcMainErlang.exists()) {
      this.targetEbin.mkdirs();
      int removed = removeFilesRecursive(this.targetEbin, BEAM_SUFFIX);
      log.info("Removed " + removed + " stale " + BEAM_SUFFIX + "-files from " + this.targetEbin);
      options.addAll(Arrays.asList(new String[]{ "+report_errors", "+report_warnings" }));
      compiled += compile(this.srcMainErlang, this.targetEbin, inc, ERL_SUFFIX, BEAM_SUFFIX, options);
    }
    if (compiled == 0) {
      log.info("No sources to compile");
    }
    else {
      log.info("Compiled " + compiled + " files");
    }
  }
}
