package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} outputs the build information ascertained by maven to
 * compile Erlang sources. The output is supposed to enable other tools (like
 * emacs) to reconstruct compilation commands.
 * </p>
 * <p>
 * The output is logged at info level and contains:
 * <ul>
 * <li>include directories that may contain erlang header files</li>
 * <li>code path directories including those of dependend projects in the target
 * directory of the maven build</li>
 * </ul>
 * The output consists of one line per item. Each item consists of two elements:
 * <code>InfoType ": " InfoValue</code> where <code>InfoType</code> is one of:
 * <ul>
 * <li><code>include_dir</code></li>
 * <li><code>code_path</code></li>
 * </ul>
 * <code>InfoValue</code> is a file path. Both elements a seperated by the
 * string ": " (colon, space).
 * </p>
 * 
 * @goal show-build-info
 * @execute phase="compile"
 * @author Sven Heyll <sven.heyll@gmail.com>
 * @since 2.0.0
 */
public class ShowBuildInfo extends ErlangMojo {

  private static final String INCLUDE_DIRS = "include_dir";
  private static final String CODE_PATHS = "code_path";

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" B U I L D - I N F O R M A T I O N");
    log.info(MavenUtils.SEPARATOR);
    List<File> includeDirs = FileUtils.getIncludeDirs(p);
    logKeyValues(log, INCLUDE_DIRS, includeDirs);
    List<File> codePaths = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    logKeyValues(log, CODE_PATHS, codePaths);
  }

  private void logKeyValues(Log log, String key, List<File> values) {
    for (File f : values) {
      log.info(key + ": " + f.getAbsolutePath());
    }
  }
}
