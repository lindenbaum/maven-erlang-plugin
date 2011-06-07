package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
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
 * Compile erlang test sources and recompile erlang sources with the options
 * {@code debug_info}, {@code export_all} and <code>{d, 'TEST'}</code>. This
 * will also compile the supporting erlang sources provided along with the
 * plugin.
 * 
 * @goal test-compile
 * @phase test-compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class TestCompiler extends ErlangMojo {
  /**
   * Setting this to {@code true} will skip the test compilation.
   * 
   * @parameter expression="${skipTests}" default-value=false
   */
  private boolean skipTests;

  /**
   * Additional compiler options (comma separated) for test compilation that are
   * directly passed to <code>compile:file/2</code>, e.g. <code>"{d, Macro},
   * nowarn_unused_function"</code>. Note: The user may not specifiy one of the
   * {@code report} options since the {@link Mojo} itself uses the
   * {@code return} option internally. Warnings and Errors will be printed
   * without specifying extra options.
   * 
   * @parameter expression="${testCompilerOptions}"
   */
  private String testCompilerOptions;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" T E S T - C O M P I L E R");
    log.info(MavenUtils.SEPARATOR);

    if (this.skipTests) {
      log.info("Test compilation is skipped.");
      return;
    }

    File out = p.targetLayout().testEbin();
    FileUtils.ensureDirectories(out);

    List<File> files = new ArrayList<File>();
    for (File d : p.sourceLayout().testSrcs()) {
      files.addAll(FileUtils.getFilesRecursive(d, ErlConstants.ERL_SUFFIX));
    }
    if (!files.isEmpty()) {
      Collection<File> testSupportFiles = p.testSupportScripts();
      extractTestSupportFiles(getClass(), testSupportFiles);
      files.addAll(testSupportFiles);

      List<String> options = new ArrayList<String>();
      options.add("debug_info");
      options.add("export_all");
      options.add("{d, 'TEST'}");
      if (this.testCompilerOptions != null && !this.testCompilerOptions.isEmpty()) {
        log.info("Using additinal test compiler options: " + this.testCompilerOptions);
        options.add(this.testCompilerOptions);
      }

      Script<CompilerResult> script = new BeamCompilerScript(files, out, p.testIncludePaths(), options);
      CompilerResult result = MavenSelf.get(p.cookie()).exec(p.testNode(), script);

      List<File> compiled = result.getCompiled();
      List<File> failed = result.getFailed();
      List<String> errors = result.getErrors();
      List<String> warnings = result.getWarnings();

      if (compiled.size() > 0) {
        log.debug("Compiled:");
        MavenUtils.logCollection(log, LogLevel.DEBUG, compiled, " * ");
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

      log.info("Successfully compiled the project test sources.");
    }
    else {
      log.info("No test source files to compile.");
    }
  }

  /**
   * Extract the test support files needed for test execution in order to
   * compile them with the test modules.
   */
  private static void extractTestSupportFiles(Class<?> caller, Collection<File> testSupportFiles) throws MojoExecutionException {
    String path = "/" + caller.getPackage().getName().replace(".", "/");
    for (File file : testSupportFiles) {
      FileUtils.extractFileFromClassPath(caller, path, file.getName(), file);
    }
  }
}
