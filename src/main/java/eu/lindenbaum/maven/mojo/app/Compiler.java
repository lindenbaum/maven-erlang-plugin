package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.BeamCompilerScript;
import eu.lindenbaum.maven.erlang.CompilerResult;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * This {@link Mojo} compiles the projects erlang sources.
 * 
 * @goal compile
 * @phase compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class Compiler extends ErlangMojo {
  /**
   * Additional compiler options (comma separated) for compilation that are
   * directly passed to <code>compile:file/2</code>, e.g. <code>"debug_info,
   * nowarn_unused_function"</code>. Note: The user may not specifiy one of the
   * {@code report} options since the {@link Mojo} itself uses the
   * {@code return} option internally. Warnings and Errors will be printed
   * without specifying extra options.
   * 
   * @parameter expression="${compilerOptions}"
   */
  private String compilerOptions;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" C O M P I L E R");
    log.info(MavenUtils.SEPARATOR);

    File ebin = p.targetLayout().ebin();
    FileUtils.ensureDirectories(ebin);

    List<File> files = FileUtils.getFilesRecursive(p.sourceLayout().src(), ErlConstants.ERL_SUFFIX);
    if (!files.isEmpty()) {

      List<String> options = new ArrayList<String>();
      if (this.compilerOptions != null && !this.compilerOptions.isEmpty()) {
        log.info("Using additional compiler options: " + this.compilerOptions);
        options.add(this.compilerOptions);
      }

      Script<CompilerResult> script = new BeamCompilerScript(files, ebin, p.includePaths(), options);
      CompilerResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);

      List<File> compiled = result.getCompiled();
      List<File> failed = result.getFailed();
      List<String> errors = result.getErrors();
      List<String> warnings = result.getWarnings();

      if (compiled.size() > 0) {
        log.info("Compiled:");
        MavenUtils.logCollection(log, LogLevel.INFO, compiled, " * ");
      }
      if (errors.size() > 0) {
        log.error("Errors:");
        MavenUtils.logCollection(log, LogLevel.ERROR, errors, "");
      }
      if (warnings.size() > 0) {
        log.warn("Warnings:");
        MavenUtils.logCollection(log, LogLevel.WARN, warnings, "");
      }

      if (failed.size() > 0) {
        throw new MojoFailureException("Failed to compile " + failed + ".");
      }
      log.info("Successfully compiled the project sources.");
    }
    else {
      log.info("No source files to compile.");
    }
  }
}
