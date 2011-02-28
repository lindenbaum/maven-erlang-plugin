package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.GenericScriptResult;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.UploadScript;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Recompiles the sources of the project and uploads them onto a configured
 * (remote) node along with the projects application file which will be loaded
 * using <code>application:load/1</code>. The modules will be purged as if
 * <code>c:c/1</code> would have been called on a shell.
 * 
 * @goal upload
 * @execute phase="package" lifecycle="upload"
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
    log.info(" U P L O A D E R");
    log.info(MavenUtils.SEPARATOR);

    PackagingType packagingType = p.packagingType();
    if (PackagingType.ERLANG_OTP != packagingType && PackagingType.ERLANG_STD != packagingType) {
      log.info("Nothing to do for packaging " + packagingType + ".");
      return;
    }

    if (p.node().equals(this.remote)) {
      log.error("Target node and backend node must not be the same node.");
      throw new MojoExecutionException("Target node equals backend node.");
    }

    List<File> modules = FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.BEAM_SUFFIX);
    List<File> applicationFiles = FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.APP_SUFFIX);
    if (this.withDependencies) {
      modules.addAll(FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX));
      applicationFiles.addAll(FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.APP_SUFFIX));
    }

    String target = "'" + this.remote + "'";
    Script<GenericScriptResult> script = new UploadScript(target, modules, applicationFiles);
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    if (result.success()) {
      log.info("Successfully uploaded files to " + target);
      result.logOutput(log);
    }
    else {
      log.error("Uploading files to " + target + " failed.");
      result.logOutput(log);
      throw new MojoExecutionException("Uploading files failed.");
    }
  }
}
