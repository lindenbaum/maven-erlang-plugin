package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.Collection;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
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
 * <li>source folder for main erlang sources</li>
 * <li>source folder for unit-test erlang sources</li>
 * <li>include directories that may contain erlang header files</li>
 * <li>code path directories including those of dependend projects in the target
 * directory of the maven build</li>
 * </ul>
 * The output consists of one line per item. Each item consists of two elements:
 * <code>InfoType ": " InfoValue</code> where <code>InfoType</code> is one of:
 * <ul>
 * <li><code>src_dir</code></li>
 * <li><code>test_src_dir</code></li>
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
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.0.0
 */
public class ShowBuildInfo extends ErlangMojo {

  private static final String SOURCE_DIR = "src_dir";
  private static final String TEST_SOURCE_DIRS = "test_src_dir";
  private static final String INCLUDE_DIRS = "include_dir";
  private static final String CODE_PATHS = "code_path";

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" B U I L D - I N F O R M A T I O N");
    log.info(MavenUtils.SEPARATOR);

    PackagingType packagingType = p.packagingType();
    if (packagingType == PackagingType.ERLANG_REL) {
      throw new MojoExecutionException("Mojo does not support packaging type " + packagingType + ".");
    }

    logKeyValue(log, SOURCE_DIR, p.sourceLayout().src());
    logKeyValues(log, TEST_SOURCE_DIRS, p.sourceLayout().testSrcs());
    logKeyValues(log, INCLUDE_DIRS, p.includePaths(false));
    logKeyValues(log, CODE_PATHS, p.codePaths(false));
  }

  private void logKeyValues(Log log, String key, Collection<File> values) {
    for (File f : values) {
      logKeyValue(log, key, f);
    }
  }

  public void logKeyValue(Log log, String key, File f) {
    log.info(key + ": " + f.getAbsolutePath());
  }
}
