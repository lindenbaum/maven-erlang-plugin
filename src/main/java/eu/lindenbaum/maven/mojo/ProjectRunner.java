package eu.lindenbaum.maven.mojo;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.GenericScriptResult;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.StartApplicationScript;
import eu.lindenbaum.maven.erlang.StartResult;
import eu.lindenbaum.maven.erlang.StopApplicationScript;
import eu.lindenbaum.maven.erlang.UploadScript;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import com.ericsson.otp.erlang.OtpPeer;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * A {@link Mojo} that runs erlang applications packaged with packaging type
 * {@link PackagingType#ERLANG_OTP} or {@link PackagingType#ERLANG_STD} on a
 * specific (remote) node. This will start the application with all dependent
 * applications and all modules pre-loaded. When finished the {@link Mojo} will
 * stop and unload the started applications.
 * </p>
 * <p>
 * TODO: release projects cannot be run
 * </p>
 * 
 * @goal run
 * @execute phase="package" lifecycle="run"
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

    PackagingType packagingType = p.packagingType();
    if (PackagingType.ERLANG_OTP == packagingType || PackagingType.ERLANG_STD == packagingType) {
      runApplication(log, p, this.remote, this.withDependencies);
    }
    else {
      throw new MojoExecutionException("Mojo does not (yet) support packaging type " + packagingType + ".");
    }
  }

  private static void runApplication(Log log, Properties p, String remote, boolean withDependencies) throws MojoExecutionException {
    boolean runOnRemoteNode = remote != null;
    String target = runOnRemoteNode ? "'" + remote + "'" : "node()";
    String targetPeer = new OtpPeer(runOnRemoteNode ? target : p.node()).toString();

    // retrieve dependency applications to start
    List<String> applications = new ArrayList<String>();
    applications.add(p.project().getArtifactId());
    for (Artifact artifact : MavenUtils.getErlangReleaseArtifacts(p.project())) {
      applications.add(artifact.getArtifactId());
    }
    Collections.reverse(applications);

    // load needed .beam/.app files when running on remote node
    if (runOnRemoteNode) {
      List<File> modules = FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.BEAM_SUFFIX);
      List<File> applicationFiles = FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.APP_SUFFIX);
      if (withDependencies) {
        modules.addAll(FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX));
        applicationFiles.addAll(FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.APP_SUFFIX));
      }
      Script<GenericScriptResult> script = new UploadScript(target, modules, applicationFiles);
      GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
      if (!result.success()) {
        log.error("Uploading files to " + targetPeer + " failed.");
        result.logOutput(log);
        throw new MojoExecutionException("Uploading needed files failed.");
      }
    }

    // start applications
    Script<StartResult> startScript = new StartApplicationScript(target, applications);
    StartResult startResult = MavenSelf.get(p.cookie()).exec(p.node(), startScript);
    if (startResult.startSucceeded()) {
      String cookie = p.cookie() != null ? " -setcookie " + p.cookie() + " " : "";
      log.info("Application started successfully on " + targetPeer + ".");
      log.info("For a remote shell use 'erl" + cookie + " -remsh " + targetPeer + " -sname yournode'");
    }
    else {
      log.error("Failed to run project:");
      startResult.logError(log);
      throw new MojoExecutionException("Failed to run application on " + targetPeer + ".");
    }

    // wait for user input when starting applications on backend node instead of remote node
    if (!runOnRemoteNode) {
      if (startResult.startSucceeded()) {
        log.info("Press [ENTER] complete the build and shutdown the backend node.");
        try {
          System.in.read();
        }
        catch (IOException e) {
          // ignored
        }
        Script<Void> stopScript = new StopApplicationScript(startResult.getBeforeApplications());
        MavenSelf.get(p.cookie()).exec(p.node(), stopScript);
      }
    }
  }
}
