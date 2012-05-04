package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.CompilerResult;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.YrlCompilerScript;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * This mojo generates erlang sources using the `yecc' application. For more
 * information see: http://www.erlang.org/doc/man/yecc.html.
 * 
 * @goal generate-sources
 * @phase generate-sources
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class YECCParserGenerator extends ErlangMojo {

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    // generated files cannot be left stale, always clean before run
    FileUtils.removeDirectory(p.sourceLayout().yecc());
    FileUtils.ensureDirectories(p.sourceLayout().yecc());

    List<File> yrlFiles = FileUtils.getFilesRecursive(p.sourceLayout().src(), ErlConstants.YRL_SUFFIX);
    if (!yrlFiles.isEmpty()) {
      log.info(MavenUtils.SEPARATOR);
      log.info(" Y E C C");
      log.info(MavenUtils.SEPARATOR);

      Script<CompilerResult> script = new YrlCompilerScript(yrlFiles, p.sourceLayout().yecc());
      CompilerResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
      List<File> compiled = result.getCompiled();
      List<File> failed = result.getFailed();
      if (compiled.size() > 0) {
        log.info("Generated:");
        MavenUtils.logCollection(log, LogLevel.INFO, compiled, " * ");
      }
      if (failed.size() > 0) {
        throw new MojoFailureException("Failed to generate " + failed + ".");
      }
      log.info("Successfully generated the project parsers from YRLs.");
    }
    else {
      log.info("No YRL files, no parsers to generate.");
    }
  }
}
