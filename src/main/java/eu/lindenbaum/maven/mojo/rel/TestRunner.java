package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.CheckRelResult;
import eu.lindenbaum.maven.erlang.CheckRelScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.MavenUtils;

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
   * Setting this to {@code true will} will skip the test compilation.
   * 
   * @parameter expression="${skipTests}" default-value=false
   */
  private boolean skipTests;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" T E S T - R U N N E R");
    log.info(MavenUtils.SEPARATOR);

    if (this.skipTests) {
      log.info("Tests are skipped.");
      return;
    }

    List<Artifact> artifacts = MavenUtils.getErlangReleaseArtifacts(p.project());
    String releaseName = p.project().getArtifactId();
    String releaseVersion = p.project().getVersion();
    String relFileBaseName = releaseName + "-" + releaseVersion;

    File relFile = new File(p.target(), relFileBaseName + ErlConstants.REL_SUFFIX);
    CheckRelScript relScript = new CheckRelScript(relFile);
    CheckRelResult relResult = MavenSelf.get(p.cookie()).exec(p.node(), relScript, new ArrayList<File>());
    checkReleaseName(log, releaseName, relResult.getName());
    checkReleaseVersion(log, releaseVersion, relResult.getReleaseVersion());
    checkDependencies(log, artifacts, relResult.getApplications());

    log.info("All tests passed.");
  }

  /**
   * Checks whether all project dependencies are declared in the .rel file and
   * whether the versions match.
   */
  private static void checkDependencies(Log log, List<Artifact> expected, Map<String, String> actual) throws MojoFailureException {
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
  private static void checkReleaseName(Log log, String expected, String actual) throws MojoFailureException {
    if (!expected.equals(actual)) {
      log.error("Release name mismatch.");
      log.error("Project release name is " + expected + " while .rel name is " + actual);
      throw new MojoFailureException("Release name mismatch " + expected + " != " + actual + ".");
    }
  }

  /**
   * Checks whether the release files release version equals the projects
   * version.
   */
  private static void checkReleaseVersion(Log log, String expected, String actual) throws MojoFailureException {
    if (!expected.equals(actual)) {
      log.error("Release version mismatch.");
      log.error("Project release version is " + expected + " while .rel version is " + actual);
      throw new MojoFailureException("Release version mismatch " + expected + " != " + actual + ".");
    }
  }
}
