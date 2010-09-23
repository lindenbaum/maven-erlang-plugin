package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.REL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.REL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.ArtifactHandler;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * TODO
 * 
 * @goal prepare-release
 * @phase compile
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class PrepareReleaseMojo extends AbstractErlangMojo {
  /**
   * Command to extract the version from the .rel file.
   */
  private static final String EXTRACT_VERSION = //
  "{ok, [{release, {_ReleaseName, ReleaseVersion}, {erts, _ErtsVersion}, _Applications}]} = file:consult(\"%s\"), "
      + "io:format(ReleaseVersion), io:nl().";

  /**
   * Command to extract the applications from the .app file.
   */
  private static final String EXTRACT_APPLICATIONS = //
  "{ok, [{release, {_ReleaseName, _ReleaseVersion}, {erts, _ErtsVersion}, Applications}]} = file:consult(\"%s\"), "
      + "lists:foreach(fun(Tuple) when is_tuple(Tuple) -> " //
      + "AppName = element(1, Tuple), AppVersion = element(2, Tuple), io:format(\"~p:~s \", [AppName, AppVersion]) "//
      + "end, Applications).";

  /**
   * Options for make_script.
   * 
   * @parameter
   */
  private String[] scriptOptions;

  /**
   * Options for make_relup.
   * 
   * @parameter
   */
  private String[] relupOptions;

  /**
   * Options for the relup up from.
   * 
   * @parameter
   */
  private List<String> subsequentVersions;

  /**
   * Option for the relup down to.
   * 
   * @parameter
   */
  private List<String> previousVersions;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("?REL_VERSION", "\"" + this.project.getVersion() + "\"");

    Log log = getLog();
    this.targetEbin.mkdirs();
    int copied = copyDirectory(this.srcMainErlang, this.targetEbin, REL_FILTER, replacements);
    log.info("Copied " + copied + " release files to " + this.targetEbin.getAbsolutePath());

    String releaseName = getReleaseName(this.project.getArtifact());
    log.info("Preparing release of " + releaseName);
    File relFile = new File(this.targetEbin, releaseName + REL_SUFFIX);
    if (relFile.exists()) {
      log.info("Checking version of " + relFile);
      checkVersion(relFile);
      log.info("Checking dependencies of " + relFile);
      checkDependencies(relFile);

      List<File> previousReleases = getReleaseFiles(this.previousVersions);
      List<File> subsequentReleases = getReleaseFiles(this.subsequentVersions);
      if (!previousReleases.isEmpty() || !subsequentReleases.isEmpty()) {
        log.info("Creating relup file for " + releaseName);
        makeRelup(relFile, previousReleases, subsequentReleases);
      }
      log.info("Creating scripts for " + releaseName);
      makeScript(releaseName);
    }
    else {
      log.error("No " + REL_SUFFIX + " file found with version " + this.project.getVersion());
    }
  }

  /**
   * Creates a relup file for the .rel files of the given previous and subsequent
   * releases of this project using {@code systools:relup}.
   * 
   * @param relFile erlang release file of this release
   * @param previousReleases list of erlang release files of previous releases
   * @param subsequentReleases list of erlang release files of subsequent releases
   * @throws MojoExecutionException in case {@code systools:make_relup} fails
   * @throws MojoFailureException in case {@code systools:make_relup} fails
   */
  private void makeRelup(File relFile, List<File> previousReleases, List<File> subsequentReleases) throws MojoExecutionException,
                                                                                                  MojoFailureException {
    StringBuilder command = new StringBuilder();
    command.append("Status = systools:make_relup(\"");
    command.append(relFile.getName());
    command.append("\"");
    command.append(", [");
    for (File previousRelease : previousReleases) {
      command.append("\"");
      command.append(previousRelease.getName());
      command.append("\",");
    }
    command.deleteCharAt(command.length() - 1);
    command.append("]");
    command.append(", [");
    for (File subsequentRelease : subsequentReleases) {
      command.append("\"");
      command.append(subsequentRelease.getName());
      command.append("\",");
    }
    command.deleteCharAt(command.length() - 1);
    command.append("]");
    command.append(", [");
    command.append("{path, [\"");
    command.append(this.targetReleases.getAbsolutePath());
    command.append(File.separator);
    command.append("*");
    command.append(File.separator);
    command.append("lib");
    command.append(File.separator);
    command.append("*");
    command.append(File.separator);
    command.append("ebin\"]}");
    if (this.relupOptions != null) {
      for (String option : this.relupOptions) {
        command.append(", ");
        command.append(option);
      }
    }
    command.append("]");
    command.append("), StatusCode = case Status of ok -> 0; _ -> 1 end, erlang:halt(StatusCode).");
    Log log = getLog();
    String result = eval(log, command.toString(), getDependencies(this.targetLib), this.targetEbin);
    if (result != null && !result.isEmpty()) {
      log.info("make_relup returned " + result);
    }
  }

  /**
   * Creates the scripts for this release using {@code systools:make_script}.
   * 
   * @param relFile the erlang release file of this release
   * @throws MojoExecutionException in case {@code systools:make_script} fails
   * @throws MojoFailureException in case {@code systools:make_script} fails
   */
  private void makeScript(String releaseName) throws MojoExecutionException, MojoFailureException {
    final StringBuilder command = new StringBuilder();
    command.append("Status = systools:make_script(\"");
    command.append(releaseName);
    command.append("\"");
    if (this.scriptOptions != null) {
      command.append(", ");
      command.append(this.scriptOptions.toString());
    }
    command.append("), StatusCode = case Status of ok -> 0; _ -> 1 end, erlang:halt(StatusCode).");
    String result = eval(getLog(), command.toString(), getDependencies(this.targetLib), this.targetEbin);
    if (result != null && !result.isEmpty()) {
      getLog().info("make_script returned " + result);
    }
  }

  /**
   * Checks whether the project version equals the erlang application version.
   * 
   * @param relFile the erlang release file
   * @throws MojoExecutionException
   * @throws MojoFailureException in case of version mismatch
   */
  private void checkVersion(File relFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String name = this.project.getArtifactId();
    String version = this.project.getVersion();
    String expression = String.format(EXTRACT_VERSION, relFile.getPath(), name, name);
    String appFileVersion = eval(log, expression, getDependencies(this.targetLib));
    if (!version.equals(appFileVersion)) {
      log.error("Version mismatch.");
      log.error("Project version is " + version + " while " + REL_SUFFIX + " version is " + appFileVersion);
      throw new MojoFailureException("Version mismatch " + version + " != " + appFileVersion);
    }
  }

  /**
   * Checks whether all project dependencies are declared in the .rel file and
   * whether the versions match. 
   * 
   * @param relFile the erlang release file
   * @throws MojoExecutionException
   * @throws MojoFailureException in case of application mismatches
   */
  @SuppressWarnings("unchecked")
  private void checkDependencies(File relFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String expression = String.format(EXTRACT_APPLICATIONS, relFile.getPath());
    String dependencyStr = eval(log, expression, getDependencies(this.targetLib));
    Map<String, String> dependencies = new HashMap<String, String>();
    for (String dep : dependencyStr.split(" ")) { // "name:version name:version ..."
      String[] application = dep.split(":"); // "name:version"
      dependencies.put(application[0], application[1]);
    }
    Set<Artifact> artifacts = this.project.getArtifacts();
    for (Artifact artifact : artifacts) {
      String artifactId = artifact.getArtifactId();
      String version = dependencies.get(artifactId);
      if (version == null) {
        throw new MojoFailureException(artifactId + "is not included in " + relFile.getName());
      }
      if (!version.equals(artifact.getVersion())) {
        throw new MojoFailureException("found version mismatch for application " + artifactId);
      }
    }
  }

  /**
   * Returns a list of .rel files of other releases of this project. 
   * 
   * @param versions the versions of releases to retrieve the .rel file for
   * @return a list of existing .rel files for the requested versions
   * @throws MojoExecutionException in case an artifact cannot be extracted
   * @throws MojoFailureException in case an artifact cannot be extracted
   */
  private List<File> getReleaseFiles(List<String> versions) throws MojoExecutionException,
                                                           MojoFailureException {
    Log log = getLog();
    List<File> result = new ArrayList<File>();
    if (versions != null) {
      for (String version : versions) {
        Artifact artifact = getArtifact(this.project.getArtifact(), version);
        File artifactFile = new File(this.repository.getBasedir(), this.repository.pathOf(artifact));
        if (artifactFile.exists()) {
          File destDir = new File(this.targetReleases, artifact.getVersion());
          extract(artifact.getFile(), destDir);
          File relFile = new File(new File(destDir, "releases"), getReleaseName(artifact) + REL_SUFFIX);
          if (relFile.exists()) {
            result.add(relFile);
          }
          else {
            log.info("Could not find " + REL_SUFFIX + " file for version " + version);
          }
        }
        else {
          log.warn("Could not find artifact for version " + version);
        }
      }
    }
    return Collections.unmodifiableList(result);
  }

  /**
   * Extracts a {@code .tar.gz} archive into a specific directory.
   * 
   * @param archive to extract
   * @param destDir to extract into
   * @throws MojoExecutionException in case the archive cannot be extracted
   * @throws MojoFailureException in case the archive cannot be extracted
   */
  private void extract(File archive, File destDir) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    destDir.mkdirs();
    StringBuilder command = new StringBuilder();
    command.append("Status = erl_tar:extract(\"");
    command.append(archive.getAbsolutePath());
    command.append("\", [compressed]), ");
    command.append("StatusCode = case Status of ok -> 0; _ -> 1 end, erlang:halt(StatusCode).");
    String result = eval(log, command.toString(), getDependencies(this.targetLib), destDir);
    if (result != null && !result.isEmpty()) {
      log.info("Extracting " + archive.getPath() + " returned " + result);
    }
  }

  /**
   * Returns an {@link Artifact} object of a specific {@link Artifact} with a
   * specific version.
   * 
   * @param from to clone
   * @param version of the returned artifact
   * @return a new {@link Artifact} with the requested version
   */
  private static Artifact getArtifact(Artifact from, String version) {
    String groupId = from.getGroupId();
    String artifactId = from.getArtifactId();
    String scope = from.getScope();
    String type = from.getType();
    String classifier = from.getClassifier();
    ArtifactHandler artifactHandler = from.getArtifactHandler();
    VersionRange versionRange = VersionRange.createFromVersion(version);
    return new DefaultArtifact(groupId, artifactId, versionRange, scope, type, classifier, artifactHandler);
  }

  /**
   * Returns the release name for the given {@link Artifact}. The release name
   * consists of the artifacts id and its version. 
   * 
   * @param artifact to retrieve the release name from
   * @return a string containing the release name
   */
  private static String getReleaseName(Artifact artifact) {
    return artifact.getArtifactId() + "-" + artifact.getVersion();
  }
}
