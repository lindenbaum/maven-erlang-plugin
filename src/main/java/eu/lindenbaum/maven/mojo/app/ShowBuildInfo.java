package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MojoUtils;

import org.apache.maven.plugin.Mojo;
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
  protected void execute(Log log, Properties p) {
    log.info(MavenUtils.SEPARATOR);
    log.info(" B U I L D - I N F O R M A T I O N");
    log.info(MavenUtils.SEPARATOR);

    PackagingType packagingType = p.packagingType();
    if (PackagingType.ERLANG_OTP != packagingType && PackagingType.ERLANG_STD != packagingType) {
      log.info("Nothing to do for packaging " + packagingType + ".");
      return;
    }

    logKeyValues(log, INCLUDE_DIRS, MojoUtils.getIncludeDirectories(p));
    logKeyValues(log, CODE_PATHS, MojoUtils.getApplicationCodePaths(p));
  }

  private void logKeyValues(Log log, String key, List<File> values) {
    for (File f : values) {
      log.info(key + ": " + f.getAbsolutePath());
    }
  }
}
