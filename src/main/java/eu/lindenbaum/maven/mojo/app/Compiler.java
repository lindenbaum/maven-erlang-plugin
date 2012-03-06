package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.BeamCompilerScript;
import eu.lindenbaum.maven.erlang.CompilerResult;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.MibCompilerScript;
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
 * This {@link Mojo} compiles the projects Erlang sources and MIB files.
 * 
 * @goal compile
 * @phase compile
 * @requiresDependencyResolution compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class Compiler extends ErlangMojo {
  /**
   * Additional compiler options (comma separated) for Erlang compilation that
   * are directly passed to <code>compile:file/2</code>, e.g. <code>"debug_info,
   * nowarn_unused_function"</code>. Note: The user may not specifiy one of the
   * {@code report} options since the {@link Mojo} itself uses the
   * {@code return} option internally. Warnings and Errors will be printed
   * without specifying extra options.
   * 
   * @parameter expression="${compilerOptions}"
   */
  private String compilerOptions;

  /**
   * Optional list of source files that will be compiled first (and in the given
   * order). For example:
   * 
   * <pre>
   *   &lt;compileFirst&gt;
   *     &lt;file&gt;foo.erl&lt;/file&gt;
   *     &lt;file&gt;bar.erl&lt;/file&gt;
   *     &lt;file&gt;baz.erl&lt;/file&gt;
   *   &lt;/compileFirst&gt;
   * </pre>
   * 
   * @parameter expression="${compileFirst}"
   * @since 2.1.0
   */
  private String[] compileFirst;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" C O M P I L E R");
    log.info(MavenUtils.SEPARATOR);

    compileMIBFiles(log, p);
    compileERLFiles(log, p);
  }

  private void compileERLFiles(Log log, Properties p) throws MojoFailureException, MojoExecutionException {
    File targetEbinDir = p.targetLayout().ebin();
    FileUtils.ensureDirectories(targetEbinDir);

    List<File> erlFiles = FileUtils.getFilesRecursive(p.sourceLayout().src(), ErlConstants.ERL_SUFFIX);

    List<File> firstFiles = new ArrayList<File>();
    if (this.compileFirst != null) {
      for (String compileFirstFile : this.compileFirst) {
        File file = FileUtils.getFile(compileFirstFile, erlFiles);
        firstFiles.add(file);
        erlFiles.remove(file);
      }
    }

    if (!erlFiles.isEmpty() || !firstFiles.isEmpty()) {
      List<String> options = new ArrayList<String>();
      if (this.compilerOptions != null && !this.compilerOptions.isEmpty()) {
        log.info("Using additional compiler options: " + this.compilerOptions);
        options.add(this.compilerOptions);
      }

      List<File> includes = p.includePaths(false);
      Script<CompilerResult> script = new BeamCompilerScript(erlFiles,
                                                             firstFiles,
                                                             targetEbinDir,
                                                             includes,
                                                             options);
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

  /**
   * @since 2.2.0
   */
  private void compileMIBFiles(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    File targetPrivDir = p.targetLayout().priv();
    FileUtils.ensureDirectories(targetPrivDir);

    List<File> mibFiles = FileUtils.getFilesRecursive(p.sourceLayout().mibs(), ErlConstants.MIB_SUFFIX);
    if (!mibFiles.isEmpty()) {

      List<File> includes = p.includePaths(false);
      includes.add(p.sourceLayout().priv());
      includes.add(p.targetLayout().priv());

      Script<CompilerResult> script = new MibCompilerScript(mibFiles, targetPrivDir, includes);

      CompilerResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);

      List<File> compiled = result.getCompiled();
      List<File> failed = result.getFailed();
      List<String> errors = result.getErrors();

      if (compiled.size() > 0) {
        log.info("Compiled:");
        MavenUtils.logCollection(log, LogLevel.INFO, compiled, " * ");
      }
      if (errors.size() > 0) {
        log.error("Errors:");
        MavenUtils.logCollection(log, LogLevel.ERROR, errors, "");
      }

      if (failed.size() > 0) {
        throw new MojoFailureException("Failed to compile " + failed + ".");
      }
      log.info("Successfully compiled the project MIBs.");
    }
    else {
      log.info("No MIBs to compile.");
    }
  }
}
