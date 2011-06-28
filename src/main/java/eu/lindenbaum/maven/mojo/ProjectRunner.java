package eu.lindenbaum.maven.mojo;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.CheckRelResult;
import eu.lindenbaum.maven.erlang.CheckRelScript;
import eu.lindenbaum.maven.erlang.GenericScriptResult;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.RunProjectScript;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.UploadScript;
import eu.lindenbaum.maven.util.MavenUtils;

import com.ericsson.otp.erlang.OtpPeer;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * A {@link Mojo} that runs erlang projects on a specific (remote) node. This
 * will start the project along with all dependent applications and all modules
 * pre-loaded. In case the project is run on the backend node the build will be
 * paused to give the user the possibilty to interact with the running project.
 * </p>
 * 
 * @goal run
 * @execute phase="package" lifecycle="run"
 * @requiresDependencyResolution runtime
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class ProjectRunner extends ErlangMojo {
  /**
   * <p>
   * The name of the node to run the application on.
   * </p>
   * <p>
   * Note: The cookie of the remote node has to be set accordingly using the
   * <code>cookie</code> parameter.
   * </p>
   * 
   * @parameter expression="${remote}"
   */
  private String remote;

  /**
   * Setting this to <code>false</code> will skip the upload of the modules from
   * the (maven) dependencies on the remote node before starting the
   * application.
   * 
   * @parameter expression="${withDependencies}" default-value=true
   */
  private boolean withDependencies;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" R U N N E R");
    log.info(MavenUtils.SEPARATOR);

    boolean runOnRemoteNode = this.remote != null;
    String target = runOnRemoteNode ? "'" + this.remote + "'" : "node()";
    String targetPeer = new OtpPeer(runOnRemoteNode ? target : p.node()).toString();

    // load needed .beam/.app files when running on remote node
    if (runOnRemoteNode) {
      upload(log, p, target, targetPeer, this.withDependencies);
    }
    start(log, p, target, targetPeer);

    // wait for user input when starting applications on backend node instead of remote node
    if (!runOnRemoteNode) {
      log.info("Press [ENTER] to complete the build and shutdown the backend node.");
      try {
        System.in.read();
      }
      catch (IOException e) {
        // ignored
      }
    }
  }

  /**
   * Starts a project on a (possibly) remote node. The project may either be an
   * application or a release.
   */
  private static void start(Log log, Properties p, String target, String targetPeer) throws MojoExecutionException {
    Script<GenericScriptResult> script;
    switch (p.packagingType()) {
      case ERLANG_OTP:
      case ERLANG_STD: {
        List<String> applications = new ArrayList<String>();
        applications.add(p.project().getArtifactId());
        applications.addAll(MavenUtils.getArtifactIds(MavenUtils.getErlangReleaseArtifacts(p.project())));
        Collections.reverse(applications);
        script = new RunProjectScript(target, applications);
        break;
      }

      default: {
        Script<CheckRelResult> checkScript = new CheckRelScript(p.targetLayout().relFile());
        CheckRelResult relResult = MavenSelf.get(p.cookie()).exec(p.node(), checkScript);
        List<String> applications = new ArrayList<String>();
        applications.addAll(relResult.getApplications().keySet());
        script = new RunProjectScript(target, applications, p.targetLayout().sysConfigFile());
        break;
      }
    }
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    result.logOutput(log);
    if (result.success()) {
      String cookie = p.cookie() != null ? " -setcookie " + p.cookie() + " " : "";
      log.info("Successfully running project on " + targetPeer + ".");
      log.info("For a remote shell use 'erl" + cookie + " -remsh " + targetPeer + " -name mynode@myhost'");
    }
    else {
      throw new MojoExecutionException("Failed to run project on " + targetPeer + ".");
    }
  }

  /**
   * Uploads an application onto a remote node optionally including the
   * applications dependencies. Application resources will be uploaded into a
   * temporary directory if possible.
   */
  private static void upload(Log log, Properties p, String target, String targetPeer, boolean withDependencies) throws MojoExecutionException {
    List<File> modules = p.modules(false, withDependencies);
    List<File> applicationFiles = p.applicationFiles(withDependencies);
    List<File> resourceFiles = p.resources(false, withDependencies);
    Script<GenericScriptResult> script = new UploadScript(target, modules, applicationFiles, resourceFiles);
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    result.logOutput(log);
    if (!result.success()) {
      throw new MojoExecutionException("Failed to upload applications to " + targetPeer + ".");
    }
  }
}
