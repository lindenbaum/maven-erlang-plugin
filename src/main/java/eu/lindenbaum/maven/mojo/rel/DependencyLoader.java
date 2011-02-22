package eu.lindenbaum.maven.mojo.rel;

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
import eu.lindenbaum.maven.util.MojoUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * {@link Mojo} that first purges all dynamically loaded modules on the backend
 * node and reloads the modules provided by (unpacked) dependencies.
 * 
 * @goal reload-release-dependencies
 * @phase process-sources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class DependencyLoader extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    // clean up dynamically loaded modules on backend from previous runs
    Script<Void> purgeScript = new PurgeModulesScript();
    MavenSelf.get(p.cookie()).exec(p.node(), purgeScript);

    // reload dependency modules on backend node
    List<File> modules = FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);

    // code paths must exist when added
    List<File> codePaths = MojoUtils.getReleaseCodePaths(p);
    FileUtils.ensureDirectories(codePaths.toArray(new File[0]));

    LoadModulesScript loadScript = new LoadModulesScript(modules);
    Integer loaded = MavenSelf.get(p.cookie()).exec(p.node(), loadScript, codePaths);
    log.debug("Successfully loaded " + loaded + " .beam file(s) from dependencies.");
  }
}
