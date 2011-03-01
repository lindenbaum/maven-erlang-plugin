package eu.lindenbaum.maven.mojo;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.GenericScriptResult;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.UploadReleaseScript;
import eu.lindenbaum.maven.erlang.UploadScript;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} uploads a project onto a remote node. In case of
 * application projects this includes remote loading of the compiled application
 * modules loading of the application resource file using
 * <code>application:load/1</code>. The modules will be purged once as if
 * <code>c:c/1</code> would have been called on a shell. In case of a release
 * project the release package (<code>.tar.gz</code>) will be uploaded in the
 * remote nodes {@code releases} directory ready to be unpacked and installed
 * using the {@code release_handler}.
 * </p>
 * <p>
 * Note: Uploading of releases will require that the remote erlang process has
 * write access to its {@code releases} directory.
 * </p>
 * 
 * @goal upload
 * @execute phase="package" lifecycle="upload"
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.0.0
 */
public final class ProjectUploader extends ErlangMojo {
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
   * (maven) dependencies on the remote node. This parameter is ignored when
   * uploading release projects.
   * 
   * @parameter expression="${withDependencies}" default-value=false
   */
  private boolean withDependencies;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" U P L O A D E R");
    log.info(MavenUtils.SEPARATOR);

    if (p.node().equals(this.remote)) {
      log.error("Target node and backend node must not be the same node.");
      throw new MojoExecutionException("Target node equals backend node.");
    }

    PackagingType packagingType = p.packagingType();
    if (PackagingType.ERLANG_OTP == packagingType || PackagingType.ERLANG_STD == packagingType) {
      uploadApplication(log, p, "'" + this.remote + "'", this.withDependencies);
    }
    else {
      uploadRelease(log, p, "'" + this.remote + "'");
    }
  }

  /**
   * Uploads an application onto a remote node optionally including the
   * applications dependencies. Application resources will not be uploaded (only
   * the compiled application code and the application resource file).
   */
  private static void uploadApplication(Log log, Properties p, String target, boolean withDependencies) throws MojoExecutionException {
    List<File> modules = FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.BEAM_SUFFIX);
    List<File> applicationFiles = FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.APP_SUFFIX);
    if (withDependencies) {
      modules.addAll(FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX));
      applicationFiles.addAll(FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.APP_SUFFIX));
    }

    Script<GenericScriptResult> script = new UploadScript(target, modules, applicationFiles);
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    if (result.success()) {
      log.info("Successfully uploaded application to " + target + ".");
      result.logOutput(log);
    }
    else {
      log.error("Uploading application to " + target + " failed.");
      result.logOutput(log);
      throw new MojoExecutionException("Uploading application failed.");
    }
  }

  /**
   * Uploads a release package onto a remote node. The release package will be
   * written to the remote node's {@code releases} directory.
   */
  private static void uploadRelease(Log log, Properties p, String target) throws MojoExecutionException {
    String releaseName = p.project().getArtifactId();
    String releaseFileBaseName = releaseName + "-" + p.project().getVersion();
    File releaseTarGz = new File(p.target(), releaseFileBaseName + ErlConstants.TARGZ_SUFFIX);
    if (!releaseTarGz.isFile()) {
      throw new MojoExecutionException("Could not find packaged release " + releaseTarGz.getPath());
    }

    Script<GenericScriptResult> script = new UploadReleaseScript(target, releaseTarGz);
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    if (result.success()) {
      log.info("Successfully uploaded release to " + target + ".");
    }
    else {
      log.error("Uploading release to " + target + " failed.");
      result.logOutput(log);
      throw new MojoExecutionException("Uploading release failed.");
    }
  }
}
