package eu.lindenbaum.maven.mojo;

import java.io.File;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MojoUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * {@link Mojo} that starts the erlang node used as a backend for rpcs made by
 * the plugin. The node will only be started if it is not already running. The
 * node will be shutdown when the executing JVM exits. This is done by a
 * {@link Runtime#addShutdownHook(Thread)} which will only be added <b>once</b>
 * each JVM execution.
 * 
 * @goal initialize
 * @phase initialize
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class BackendInitializer extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    File buildDir = p.targetLayout().base();
    FileUtils.ensureDirectories(buildDir);
    File backendLog = p.targetLayout().backendLog();
    MojoUtils.startBackend(log, p.erlCommand(), p.node(), p.cookie(), buildDir, backendLog);
  }
}
