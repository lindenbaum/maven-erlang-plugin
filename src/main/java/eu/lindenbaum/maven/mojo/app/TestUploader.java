package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.GenericScriptResult;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.UploadScript;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} uploads an application's modules compiled for test
 * execution and the application's tests onto a remote node. This will also
 * upload the application's resource files making them available to the remote
 * node using <code>code:priv_dir/1</code>. The modules will be purged once as
 * if <code>c:c/1</code> would have been called on a shell.
 * </p>
 * 
 * @goal upload-tests
 * @execute phase="test-compile" lifecycle="testupload"
 * @requiresDependencyResolution test
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.0.0
 */
public final class TestUploader extends ErlangMojo {
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
    log.info(" T E S T S - U P L O A D E R");
    log.info(MavenUtils.SEPARATOR);

    if (p.node().equals(this.remote)) {
      log.error("Target node and backend node must not be the same node.");
      throw new MojoExecutionException("Target node equals backend node.");
    }

    PackagingType packagingType = p.packagingType();
    if (packagingType == PackagingType.ERLANG_REL) {
      throw new MojoExecutionException("Mojo does not support packaging type " + packagingType + ".");
    }

    String target = "'" + this.remote + "'";
    List<File> modules = p.modules(true, this.withDependencies);
    List<File> resources = p.resources(true, this.withDependencies);
    Script<GenericScriptResult> script = new UploadScript(target, modules, new ArrayList<File>(), resources);
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    result.logOutput(log);
    if (!result.success()) {
      throw new MojoExecutionException("Uploading tests failed.");
    }
  }
}
