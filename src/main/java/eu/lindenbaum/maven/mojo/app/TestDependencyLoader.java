package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.LoadModulesScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.PurgeModulesScript;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * {@link Mojo} that first purges and all dynamically loaded modules on the
 * test backend node and reloads the modules provided by (unpacked)
 * dependencies.
 * 
 * @goal reload-test-dependencies
 * @phase process-test-sources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestDependencyLoader extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    // clean up dynamically loaded modules on backend from previous runs
    Script<Void> purgeScript = new PurgeModulesScript();
    MavenSelf.get(p.testCookie()).exec(p.testNode(), purgeScript);

    // reload dependency modules on backend node
    List<File> modules = FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    List<File> codePaths = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    if (!modules.isEmpty()) {
      LoadModulesScript loadScript = new LoadModulesScript(modules);
      Integer loaded = MavenSelf.get(p.testCookie()).exec(p.testNode(), loadScript, codePaths);
      log.debug("Successfully loaded " + loaded + " .beam file(s) from dependencies.");
    }
  }
}
