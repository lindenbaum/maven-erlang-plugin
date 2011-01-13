package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.MakeTarScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.SystoolsScriptResult;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * This {@link Mojo} packages creates a release {@code .tar.gz} using the erlang
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
   * @parameter expression="${scriptOptions}"
   */
  private String tarOptions;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" P A C K A G E R");
    log.info(MavenUtils.SEPARATOR);

    String releaseName = p.project().getArtifactId();
    String releaseVersion = p.project().getVersion();
    String relFileBaseName = releaseName + "-" + releaseVersion;

    File relFile = new File(p.target(), relFileBaseName + ErlConstants.REL_SUFFIX);
    List<File> codePaths = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    codePaths.add(p.target());

    Script<SystoolsScriptResult> script = new MakeTarScript(relFile, p.target(), this.tarOptions);
    SystoolsScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script, codePaths);
    result.logOutput(log);
    if (!result.success()) {
      throw new MojoFailureException("Could not create release package.");
    }

    File relTarGz = new File(p.target(), relFileBaseName + ErlConstants.TARGZ_SUFFIX);
    if (!relTarGz.isFile()) {
      throw new MojoFailureException("Could not find packaged release " + relTarGz.getPath());
    }
    p.project().getArtifact().setFile(relTarGz);
    log.info("Successfully created release package: " + relTarGz.getName());
  }
}
