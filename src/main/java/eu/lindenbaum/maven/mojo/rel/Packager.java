package eu.lindenbaum.maven.mojo.rel;

import java.io.File;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.GenericScriptResult;
import eu.lindenbaum.maven.erlang.MakeTarScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * This {@link Mojo} packages/creates a release {@code .tar.gz} using the erlang
 * {@code systools} modules. The build can be customized by providing additional
 * options through the {@link #tarOptions} parameter.
 * 
 * @goal package-release
 * @phase package
 * @author Paul Guyot
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class Packager extends ErlangMojo {
  /**
   * Additional options for {@code systools:make_tar/2} (comma separated). Note:
   * The <code>silent</code> option is passed by the plugin, do not overwrite.
   * 
   * @parameter expression="${tarOptions}"
   */
  private String tarOptions;

  /**
   * <p>
   * Setting this to {@code true} will include the backend node's erlang
   * emulator into the release package.
   * </p>
   * <p>
   * Note: The resulting package is then highly system dependent since it
   * contains the erlang emulator (C code) from the backend nodes erlang
   * installation.
   * </p>
   * 
   * @parameter expression="${includeErts}" default-value=false
   */
  private boolean includeErts;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" P A C K A G E R");
    log.info(MavenUtils.SEPARATOR);

    String options = this.tarOptions != null ? this.tarOptions : "";
    if (this.includeErts) {
      String erts = "{erts,code:root_dir()}";
      if (options.isEmpty()) {
        options = erts;
      }
      else {
        options = erts + "," + options;
      }
    }

    File relFile = p.targetRelFile();
    Script<GenericScriptResult> script = new MakeTarScript(relFile, p.target(), options);
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    result.logOutput(log);
    if (!result.success()) {
      throw new MojoFailureException("Could not create release package.");
    }

    File releaseTarGz = p.projectArtifactFile();
    if (!releaseTarGz.isFile()) {
      throw new MojoFailureException("Could not find packaged release " + releaseTarGz.getPath());
    }

    p.project().getArtifact().setFile(releaseTarGz);
    log.info("Successfully created release package:");
    log.info(releaseTarGz.toString());
  }
}
