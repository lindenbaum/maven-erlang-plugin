package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.CheckRelResult;
import eu.lindenbaum.maven.erlang.CheckRelScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.RuntimeInfo;
import eu.lindenbaum.maven.erlang.RuntimeInfoScript;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * This {@link Mojo} checks the release file for plausability. This is done by
 * checking the release version, the release name as well as checking all
 * dependencies for correct versioning and existence.
 * 
 * @goal test-release
 * @phase test
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestRunner extends ErlangMojo {
  /**
   * Setting this to {@code true will} will skip comparison of the OTP release
   * version against the required OTP version. The release build will fail if
   * the backend node doesn't run the required erlang/OTP release specified by
   * {@link #otpRelease}.
   * 
   * @parameter expression="${skipReleaseTest}" default-value=false
   * @see #otpRelease
   */
  private boolean skipReleaseTest;

  /**
   * <p>
   * This <b>must</b> be set to the erlang/OTP release version this release has
   * to be based on. The version must be given as it would be returned by
   * <code>erlang:system_info(otp_release)</code>. All standard OTP dependencies
   * like {@code kernel}, {@code stdlib}, ... will be configured to the version
   * of the configured erlang/OTP release.
   * </p>
   * <p>
   * In order to do this the executing backend node has to run the required OTP
   * release. Release packaging will fail if the backend node doesn't do so
   * unless the {@link #skipReleaseTest} parameter is specified.
   * </p>
   * 
   * @parameter expression="${otpRelease}"
   * @required
   */
  private String otpRelease;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" T E S T - R U N N E R");
    log.info(MavenUtils.SEPARATOR);

    RuntimeInfoScript infoScript = new RuntimeInfoScript();
    RuntimeInfo runtimeInfo = MavenSelf.get(p.cookie()).exec(p.node(), infoScript);

    if (!this.skipReleaseTest) {
      checkOtpReleaseVersion(log, this.otpRelease, runtimeInfo.getOtpRelease());
    }
    else {
      log.warn("erlang/OTP release version check is skipped.");
      log.warn("Standard erlang/OTP applications will be included from release '"
               + runtimeInfo.getOtpRelease() + "'.");
    }

    String releaseName = p.project().getArtifactId();
    String releaseVersion = p.project().getVersion();
    Set<Artifact> artifacts = MavenUtils.getErlangReleaseArtifacts(p.project());

    File relFile = p.targetRelFile();
    CheckRelScript relScript = new CheckRelScript(relFile);
    CheckRelResult relResult = MavenSelf.get(p.cookie()).exec(p.node(), relScript);
    if (!relResult.success()) {
      log.error("Failed to consult file");
      MavenUtils.logContent(log, LogLevel.ERROR, relFile);
      throw new MojoFailureException("Failed to consult .rel file.");
    }

    checkReleaseName(log, relFile, releaseName, relResult.getName());
    checkReleaseVersion(log, relFile, releaseVersion, relResult.getReleaseVersion());
    checkDependencies(log, artifacts, relResult.getApplications());

    log.info("All tests passed.");
  }

  /**
   * Checks whether the required erlang/OTP release version needed to build the
   * release is actually available to the backend node.
   */
  private static void checkOtpReleaseVersion(Log log, String expected, String actual) throws MojoFailureException {
    if (!actual.equals(expected)) {
      log.error("Backend node does not run the required erlang/OTP release.");
      log.error("Required release is '" + expected + "' while backend node runs '" + actual + "'.");
      String msg = "Required erlang/OTP release not available " + expected + " != " + actual + ".";
      throw new MojoFailureException(msg);
    }
  }

  /**
   * Checks whether all project dependencies are declared in the .rel file and
   * whether the versions match.
   */
  private static void checkDependencies(Log log, Collection<Artifact> expected, Map<String, String> actual) throws MojoFailureException {
    boolean errors = false;
    for (Artifact artifact : expected) {
      String version = actual.get(artifact.getArtifactId());
      if (version == null) {
        log.error(artifact.getArtifactId() + " is not included in the .rel file.");
        errors = true;
      }
      else if (!version.equals(artifact.getVersion())) {
        log.error("Version mismatch for " + artifact.getArtifactId() + ": " + expected + " != " + actual);
        errors = true;
      }
    }
    if (actual.get("kernel") == null || actual.get("stdlib") == null) {
      log.error("The mandatory applications 'kernel' and 'stdlib' must be part of every release.");
      errors = true;
    }
    if (errors) {
      throw new MojoFailureException("Checking dependencies failed, see previous output for detailed description.");
    }
  }

  /**
   * Checks whether the release files release name equals the projects release
   * name.
   */
  private static void checkReleaseName(Log log, File relFile, String expected, String actual) throws MojoFailureException {
    if (!expected.equals(actual)) {
      log.error("Release name mismatch.");
      log.error("Project release name is " + expected + " while .rel name is " + actual);
      MavenUtils.logContent(log, LogLevel.ERROR, relFile);
      throw new MojoFailureException("Release name mismatch " + expected + " != " + actual + ".");
    }
  }

  /**
   * Checks whether the release files release version equals the projects
   * version.
   */
  private static void checkReleaseVersion(Log log, File relFile, String expected, String actual) throws MojoFailureException {
    if (!expected.equals(actual)) {
      log.error("Release version mismatch.");
      log.error("Project release version is " + expected + " while .rel version is " + actual);
      MavenUtils.logContent(log, LogLevel.ERROR, relFile);
      throw new MojoFailureException("Release version mismatch " + expected + " != " + actual + ".");
    }
  }
}
