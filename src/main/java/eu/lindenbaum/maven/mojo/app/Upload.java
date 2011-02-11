package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.UploadModulesScript;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Recompiles the sources of the project and uploads them onto a configured
 * (remote) node.
 * 
 * @goal upload
 * @execute phase="compile"
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.0.0
 */
public final class Upload extends ErlangMojo {
  /**
   * <p>
   * The name of the node to upload the compiled code to.
   * </p>
   * <p>
   * Note: The cookie of the remote node has to be set accordingly using the
   * <code>cookie</code> parameter.
   * </p>
   * 
   * @parameter expression="${remote}"
   * @required
   */
  private String remote;

  /**
   * Setting this to <code>true</code> will also upload all modules from the
   * (maven) dependencies on the remote node.
   * 
   * @parameter expression="${withDependencies}" default-value=false
   */
  private boolean withDependencies;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" U P L O A D");
    log.info(MavenUtils.SEPARATOR);

    PackagingType packagingType = p.packagingType();
    if (PackagingType.ERLANG_OTP != packagingType && PackagingType.ERLANG_STD != packagingType) {
      log.info("Nothing to do for packaging " + packagingType + ".");
      return;
    }

    List<File> modules = FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.BEAM_SUFFIX);
    if (this.withDependencies) {
      modules.addAll(FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX));
    }

    Script<String> script = new UploadModulesScript(this.remote, modules);
    String result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    if (!"ok".equals(result)) {
      log.error("Uploading modules failed: " + result + ".");
      throw new MojoExecutionException("Uploading modules failed: " + result + ".");
    }
    else {
      log.info("Successfully uploaded " + modules.size() + " modules to " + this.remote);
    }
  }
}
