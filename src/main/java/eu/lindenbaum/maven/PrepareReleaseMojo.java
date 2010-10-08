package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.EBIN_DIRECTORY;
import static eu.lindenbaum.maven.util.ErlConstants.REL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.REL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;
import static eu.lindenbaum.maven.util.FileUtils.removeDirectory;
import static eu.lindenbaum.maven.util.MavenUtils.getArtifact;
import static eu.lindenbaum.maven.util.MavenUtils.getArtifactFile;
import static eu.lindenbaum.maven.util.MavenUtils.getReleaseName;
import static org.codehaus.plexus.util.FileUtils.fileWrite;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import eu.lindenbaum.maven.util.TarGzUnarchiver;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} prepares the packaging of a release {@code .tar.gz} package
 * by creating the release upgrade/downgrade scripts as well as the boot and
 * start scripts. The scripts are created using the erlang {@code systools}
 * modules {@code make_relup} and {@code make_script} functions.
 * </p>
 * <p>
 * The builds can be customized by passing user options through the
 * {@link #scriptOptions} and {@link #relupOptions} parameters in the specific
 * project pom. In order to manage the project over the project pom there is the
 * possibility to let the {@link Mojo} automatically fill in values from the
 * project pom into the {@code .rel} file. This can be done by using one of the
 * supported variables into the release file. Below is a list of supported
 * variables and their substitutions:
 * </p>
 * <ul>
 * <li><code>${ARTIFACT}</code>: the projects artifact id (atom)</li>
 * <li><code>${VERSION}</code>: the projects version (string)</li>
 * <li><code>${APPLICATIONS}</code>: a list of the project dependencies
 * application and version tuples</li>
 * <li><code>${<i>APPLICATION_NAME</i>}</code>: will be replaced by a string
 * representing the available application version on this host</li>
 * </ul>
 * <p>
 * In case there is no release file specified the {@link Mojo} will generate a
 * default {@code .rel} file which looks like this:
 * </p>
 * 
 * <pre>
 * {release,\n"
 *   {${ARTIFACT}, ${VERSION}},
 *   {erts, ${ERTS}},
 *   [{kernel, ${KERNEL}}, 
 *    {stdlib, ${STDLIB}}] ++ ${APPLICATIONS}}.
 * </pre>
 * <p>
 * The resulting release file will be checked for plausability regardless if
 * generated or not. This is done by checking the release version against the
 * project version and checking all dependency versions against the application
 * versions in the release file.
 * </p>
 * <p>
 * In order to create the release downgrade/upgrade script the {@link Mojo} also
 * needs a list of versions to upgrade from or to downgrade to. If the user
 * chooses to not specify both lists the release script generation will be
 * skipped.
 * </p>
 * <p>
 * TODO The generation of relup files has not yet been tested.
 * </p>
 * 
 * @goal prepare-release
 * @phase compile
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class PrepareReleaseMojo extends AbstractErlangMojo {
  private static final String EXTRACT_VERSION = // 
  " " + "{ok, [{release, {_, ReleaseVersion}, _, _}]} = file:consult(\"%s\"), " //
      + "io:format(ReleaseVersion), io:nl().";

  private static final String EXTRACT_APPLICATIONS = //
  " " + "{ok, [{release, _, _, Applications}]} = file:consult(\"%s\"), "
      + "lists:foreach(fun(Tuple) when is_tuple(Tuple) -> " //
      + "  AppName = element(1, Tuple), " //
      + "  AppVersion = element(2, Tuple), " //
      + "  io:format(\"~p:~s \", [AppName, AppVersion]) "//
      + "end, Applications).";

  private static final String DEFAULT_REL = //
  "{release,\n" //
      + "  {${ARTIFACT}, ${VERSION}},\n" //
      + "  {erts, ${ERTS}},\n" //
      + "  [{kernel, ${KERNEL}},\n" // 
      + "   {stdlib, ${STDLIB}}] ++\n"//
      + "  ${APPLICATIONS}}.";

  /**
   * Additional options for {@code systools:make_script/2}.
   * 
   * @parameter
   */
  private String[] scriptOptions;

  /**
   * Additional options for {@code systools:make_relup/4}.
   * 
   * @parameter
   */
  private String[] relupOptions;

  /**
   * A {@link List} of version {@link String}s to downgrade from when this
   * release is installed.
   * 
   * @parameter
   */
  private List<String> subsequentVersions;

  /**
   * A {@link List} of version {@link String}s to upgrade from when this release
   * is installed.
   * 
   * @parameter
   */
  private List<String> previousVersions;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("${ARTIFACT}", "\"" + this.project.getArtifactId() + "\"");
    replacements.put("${VERSION}", "\"" + this.project.getVersion() + "\"");
    replacements.put("${APPLICATIONS}", getReleaseDependencies());
    putApplicationReplacements(replacements);

    this.targetEbin.mkdirs();
    int copied = copyDirectory(this.srcMainErlang, this.targetEbin, REL_FILTER, replacements);
    log.info("Copied " + copied + " release files to " + this.targetEbin.getAbsolutePath());

    String releaseName = getReleaseName(this.project.getArtifact());
    log.info("Preparing release of " + releaseName);
    File relFile = new File(this.targetEbin, releaseName + REL_SUFFIX);
    if (!relFile.exists()) {
      try {
        fileWrite(relFile.getPath(), "UTF-8", DEFAULT_REL);
      }
      catch (IOException e) {
        throw new MojoFailureException("Could not find .rel file, also failed to create default .rel file");
      }
    }
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
    removeDirectory(this.targetReleases);
    log.info("Creating scripts for " + releaseName);
    makeScript(releaseName);
  }

  /**
   * Creates a relup file for the .rel files of the given previous and
   * subsequent releases of this project using {@code systools:relup}.
   * 
   * @param relFile erlang release file of this release
   * @param previousReleases list of erlang release files of previous releases
   * @param subsequentReleases list of erlang release files of subsequent
   *          releases
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
    command.append(EBIN_DIRECTORY);
    command.append("\"]}");
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
  private void checkDependencies(File relFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String expression = String.format(EXTRACT_APPLICATIONS, relFile.getPath());
    String dependencyStr = eval(log, expression, getDependencies(this.targetLib));
    Map<String, String> dependencies = new HashMap<String, String>();
    for (String dep : dependencyStr.split(" ")) { // "name:version name:version ..."
      String[] application = dep.split(":"); // "name:version"
      dependencies.put(application[0], application[1]);
    }
    @SuppressWarnings("unchecked")
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
  private List<File> getReleaseFiles(List<String> versions) throws MojoExecutionException {
    Log log = getLog();
    List<File> result = new ArrayList<File>();
    if (versions != null) {
      for (String version : versions) {
        Artifact artifact = getArtifact(this.project.getArtifact(), version);
        File artifactFile = getArtifactFile(artifact, this.repository);
        File destDir = new File(this.targetReleases, artifact.getVersion());
        try {
          new TarGzUnarchiver(log, destDir).extract(artifactFile);
          File relFile = new File(new File(destDir, "releases"), getReleaseName(artifact) + REL_SUFFIX);
          if (relFile.exists()) {
            result.add(relFile);
          }
          else {
            log.info("Could not find " + REL_SUFFIX + " file for version " + version);
          }
        }
        catch (IOException e) {
          // ignored
        }
      }
    }
    return Collections.unmodifiableList(result);
  }

  /**
   * Return list of application version tuples taken from the projects
   * dependency section in the form of a valid erlang list. From is
   * <code>[{"application", "version"}, ...]</code>.
   * 
   * @return a list of application and version tuples
   */
  private String getReleaseDependencies() {
    @SuppressWarnings("unchecked")
    Set<Artifact> artifacts = this.project.getArtifacts();
    int i = 0;
    StringBuilder applications = new StringBuilder("[");
    for (Artifact artifact : artifacts) {
      if (i++ != 0) {
        applications.append(",\n  ");
      }
      applications.append("{\'");
      applications.append(artifact.getArtifactId());
      applications.append("\', \"");
      applications.append(artifact.getVersion());
      applications.append("\"}");
    }
    applications.append("]");
    return applications.toString();
  }

  /**
   * Inserts version mappings of the form <code>${APP_NAME}</code> for all
   * applications available in the local OTP installation.
   * 
   * @param replacements to insert mappings into
   * @throws MojoExecutionException
   * @throws MojoFailureException
   */
  private void putApplicationReplacements(Map<String, String> replacements) throws MojoExecutionException,
                                                                           MojoFailureException {
    Log log = getLog();
    String libDirectory = eval(log, "io:format(\"~s\", [code:lib_dir()]), io:nl().");
    File[] applications = new File(libDirectory).listFiles(new FileFilter() {
      @Override
      public boolean accept(File file) {
        return file.isDirectory();
      }
    });
    for (File application : applications) {
      String[] nameVersion = application.getName().split("-");
      replacements.put("${" + nameVersion[0].toUpperCase() + "}", "\"" + nameVersion[1] + "\"");
    }
    log.debug("replacements: " + replacements.toString());
  }
}
