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
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} uploads a project onto a remote node. In case of
 * application projects this includes remote loading of the compiled application
 * modules, loading of the application resource file using
 * <code>application:load/1</code> and uploading the project's private resources
 * making them available using <code>code:priv_dir/1</code>. To achieve this the
 * temporary upload directory is added to the remote node's code path. The
 * modules will be purged once as if <code>c:c/1</code> would have been called
 * on a shell. In case of a release project the release package (
 * <code>.tar.gz</code>) will be uploaded in the remote nodes {@code releases}
 * directory ready to be unpacked and installed using the
 * {@code release_handler}.
 * </p>
 * <p>
 * Note: Uploading of releases will require that the remote erlang process has
 * write access to its {@code releases} directory.
 * </p>
 * 
 * @goal upload
 * @execute phase="package" lifecycle="upload"
 * @requiresDependencyResolution runtime
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
   * applications dependencies. Application resources will be uploaded into a
   * temporary directory if possible.
   */
  private static void uploadApplication(Log log, Properties p, String target, boolean withDependencies) throws MojoExecutionException {
    List<File> modules = p.modules(false, withDependencies);
    List<File> applicationFiles = p.applicationFiles(withDependencies);
    List<File> resourceFiles = p.resources(false, withDependencies);
    Script<GenericScriptResult> script = new UploadScript(target, modules, applicationFiles, resourceFiles);
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    result.logOutput(log);
    if (!result.success()) {
      throw new MojoExecutionException("Uploading application failed.");
    }
  }

  /**
   * Uploads a release package onto a remote node. The release package will be
   * written to the remote node's {@code releases} directory.
   */
  private static void uploadRelease(Log log, Properties p, String target) throws MojoExecutionException {
    File artifact = p.targetLayout().projectArtifact();
    if (!artifact.isFile()) {
      throw new MojoExecutionException("Could not find packaged release " + artifact.getPath());
    }

    Script<GenericScriptResult> script = new UploadReleaseScript(target, artifact);
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
