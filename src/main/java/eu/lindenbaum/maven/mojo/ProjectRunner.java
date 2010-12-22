package eu.lindenbaum.maven.mojo;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.ericsson.otp.erlang.OtpPeer;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.LoadModulesScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.StartApplicationScript;
import eu.lindenbaum.maven.erlang.StartResult;
import eu.lindenbaum.maven.erlang.StopApplicationScript;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * A {@link Mojo} that runs erlang applications packaged with packaging type
 * {@link PackagingType#ERLANG_OTP} or {@link PackagingType#ERLANG_STD}. This
 * will start the application with all dependent applications and all modules
 * pre-loaded in the plugins backend node. When finished the {@link Mojo} will
 * stop the started applications and purge the loaded modules.
 * </p>
 * <p>
 * TODO: release projects cannot be run
 * </p>
 * 
 * @goal run
 * @execute phase="package"
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class ProjectRunner extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" P R O J E C T - R U N N E R");
    log.info(MavenUtils.SEPARATOR);

    PackagingType packagingType = p.packagingType();
    if (PackagingType.ERLANG_OTP == packagingType || PackagingType.ERLANG_STD == packagingType) {
      runApplication(log, p);
    }
    else {
      log.info("Nothing to do for packaging " + packagingType + ".");
    }
  }

  private static void runApplication(Log log, Properties p) throws MojoExecutionException {
    List<File> codePaths = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    codePaths.add(p.targetEbin());

    List<File> modules = FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    modules.addAll(FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.BEAM_SUFFIX));

    LoadModulesScript loadScript = new LoadModulesScript(modules, codePaths);
    Integer loaded = MavenSelf.get(p.cookie()).exec(p.node(), loadScript);
    log.info("Successfully loaded " + loaded + " .beam file(s) into backend node.");

    List<String> applications = new ArrayList<String>();
    applications.add(p.project().getArtifactId());
    for (Artifact artifact : MavenUtils.getErlangReleaseArtifacts(p.project())) {
      applications.add(artifact.getArtifactId());
    }
    Collections.reverse(applications);
    Script<StartResult> startScript = new StartApplicationScript(codePaths, applications);
    StartResult startResult = MavenSelf.get(p.cookie()).exec(p.node(), startScript);
    if (startResult.startSucceeded()) {
      String cookie = p.cookie() != null ? " -set_cookie " + p.cookie() + " " : "";
      String peer = new OtpPeer(p.node()).toString();
      log.info("Application started successfully on backend node.");
      log.info("To connect to the node use 'erl" + cookie + " -remsh " + peer + " -sname yournode'");
      log.info("Press [ENTER] to stop the application and cleanup the backend node.");
      try {
        System.in.read();
      }
      catch (IOException e) {
        // ignored
      }
    }
    else {
      log.error("Failed to run project:");
      startResult.logError(log);
      log.error("Cleaning up.");
    }
    List<String> toPreserve = startResult.getBeforeApplications();
    Script<Void> stopScript = new StopApplicationScript(toPreserve);
    MavenSelf.get(p.cookie()).exec(p.node(), stopScript);
  }
}
