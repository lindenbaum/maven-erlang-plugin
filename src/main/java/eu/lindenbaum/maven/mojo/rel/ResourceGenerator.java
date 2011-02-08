package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.CheckAppResult;
import eu.lindenbaum.maven.erlang.CheckAppScript;
import eu.lindenbaum.maven.erlang.MakeScriptScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.RuntimeInfo;
import eu.lindenbaum.maven.erlang.RuntimeInfoScript;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.erlang.SystoolsScriptResult;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * Copies all resource files into that target directory structure. Copied
 * resources contain:
 * <ul>
 * <li>release file (*.rel)</li>
 * <li>boot scripts, etc. as returned from <code>systools:make_script</code></li>
 * </ul>
 * The build of script files can be customized by passing user options through
 * the {@link #scriptOptions} parameter. In order to manage the project over the
 * project pom there is the possibility to let the {@link Mojo} automatically
 * fill in values from the project pom into the {@code .rel} file. This can be
 * done by using one of the supported variables into the release file. Below is
 * a list of supported variables and their substitutions: </p>
 * <ul>
 * <li><code>${ARTIFACT}</code>: the projects artifact id (atom)</li>
 * <li><code>${VERSION}</code>: the projects version (string)</li>
 * <li><code>${APPLICATIONS}</code>: a comma separated listing of the project
 * dependencies application and version tuples as configured in the project's
 * {@code pom.xml}</li>
 * <li><code>${AUTODEPS}</code>: an erlang list with all transitive dependency
 * applications of the project</li>
 * <li><code>${<i>APPLICATION_NAME</i>}</code>: will be replaced by a string
 * representing the available application version on this host</li>
 * </ul>
 * 
 * @goal generate-release-resources
 * @phase generate-resources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Gregory Haskins <ghaskins@novell.com>
 */
public final class ResourceGenerator extends ErlangMojo {
  /**
   * Additional options for {@code systools:make_script/2} (comma separated).
   * Note: The <code>silent</code> option is passed by the plugin, do not
   * overwrite.
   * 
   * @parameter expression="${scriptOptions}"
   */
  private String scriptOptions;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    RuntimeInfoScript infoScript = new RuntimeInfoScript();
    RuntimeInfo runtimeInfo = MavenSelf.get(p.cookie()).exec(p.node(), infoScript, new ArrayList<File>());
    File otpLibDirectory = runtimeInfo.getLibDirectory();

    Set<Artifact> artifacts = MavenUtils.getErlangReleaseArtifacts(p.project());

    String releaseName = p.project().getArtifactId();
    String releaseVersion = p.project().getVersion();
    String releaseFileBase = releaseName + "-" + releaseVersion;

    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("${ARTIFACT}", "\"" + releaseName + "\"");
    replacements.put("${VERSION}", "\"" + releaseVersion + "\"");
    replacements.put("${ERTS}", "\"" + runtimeInfo.getVersion() + "\"");
    replacements.put("${APPLICATIONS}", ErlUtils.toApplicationTuples(artifacts.toArray(new Artifact[0])));

    Map<String, CheckAppResult> appInfos = getAppInfos(p, p.targetLib(), otpLibDirectory);
    Set<CheckAppResult> autoDependencies = getDependencies(getArtifactIdCollection(artifacts), appInfos);

    log.debug("Found dependencies: " + autoDependencies);

    CheckAppResult[] autoDependenciesArray = autoDependencies.toArray(new CheckAppResult[0]);
    replacements.put("${AUTODEPS}", "[" + ErlUtils.toApplicationTuples(autoDependenciesArray) + "]");
    replacements.putAll(getApplicationMappings(appInfos));

    log.debug("Created mappings: " + replacements);

    File srcRelFile = new File(p.ebin(), releaseName + ErlConstants.REL_SUFFIX);
    File destRelFile = new File(p.target(), releaseFileBase + ErlConstants.REL_SUFFIX);
    FileUtils.copyFile(srcRelFile, destRelFile, replacements);
    log.debug("Copied release file to " + destRelFile + " .");

    File srcRelupFile = new File(p.ebin(), releaseName + ErlConstants.RELUP_SUFFIX);
    if (srcRelupFile.isFile()) {
      File destRelupFile = new File(p.target(), releaseFileBase + ErlConstants.RELUP_SUFFIX);
      FileUtils.copyFile(srcRelupFile, destRelupFile, replacements);
      log.debug("Copied release upgrade file to " + destRelupFile + " .");
    }

    List<File> codePaths = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    codePaths.add(p.target());

    Script<SystoolsScriptResult> script = new MakeScriptScript(destRelFile, p.target(), this.scriptOptions);
    SystoolsScriptResult makeScriptResult = MavenSelf.get(p.cookie()).exec(p.node(), script, codePaths);
    makeScriptResult.logOutput(log);
    if (!makeScriptResult.success()) {
      throw new MojoFailureException("Could not create boot scripts.");
    }
  }

  /**
   * Returns a {@link Map} containing version mappings of the form
   * <code>${APP_NAME}</code> for all applications available in the local OTP
   * installation.
   */
  private static Map<String, String> getApplicationMappings(Map<String, CheckAppResult> appInfos) {
    HashMap<String, String> mappings = new HashMap<String, String>();
    for (Entry<String, CheckAppResult> entry : appInfos.entrySet()) {
      CheckAppResult appResult = entry.getValue();
      String key = "${" + appResult.getName().toUpperCase() + "}";
      mappings.put(key, "\"" + appResult.getVersion() + "\"");
    }
    return mappings;
  }

  /**
   * Returns a {@link Map} containing {@link CheckAppResult} mappings for all
   * applications available in the given lib directories installation.
   */
  private static Map<String, CheckAppResult> getAppInfos(Properties p, File... libDirectories) throws MojoExecutionException {
    Map<String, CheckAppResult> applications = new HashMap<String, CheckAppResult>();
    for (File libDirectory : libDirectories) {
      for (File appFile : FileUtils.getFilesRecursive(libDirectory, ErlConstants.APP_SUFFIX)) {
        Script<CheckAppResult> script = new CheckAppScript(appFile);
        CheckAppResult result = MavenSelf.get(p.cookie()).exec(p.node(), script, new ArrayList<File>());
        applications.put(result.getName(), result);
      }
    }
    return applications;
  }

  /**
   * Converts a {@link Collection} of {@link Artifact}s into a
   * {@link Collection} containing their artifactIds.
   */
  private static Collection<String> getArtifactIdCollection(Collection<Artifact> artifacts) {
    Collection<String> result = new ArrayList<String>();
    for (Artifact artifact : artifacts) {
      result.add(artifact.getArtifactId());
    }
    return result;
  }

  /**
   * Returns the {@link CheckAppResult} for a specific application/artifactId.
   * In case the {@link Map} does not contain a matching {@link CheckAppResult}
   * an {@link IllegalStateException} will be thrown.
   */
  private static CheckAppResult resolve(String application, Map<String, CheckAppResult> appInfos) {
    CheckAppResult result = appInfos.get(application);
    if (result == null) {
      throw new IllegalStateException("Can't resolve application '" + application + "'.");
    }
    return result;
  }

  /**
   * Returns a {@link Set} containing the transitive dependencies for a list of
   * erlang artifactIds/applications. This will at least contain the 'kernel'
   * and 'stdlib' applications.
   */
  private static Set<CheckAppResult> getDependencies(Collection<String> artifactIds,
                                                     Map<String, CheckAppResult> appInfos) {
    LinkedList<String> todo = new LinkedList<String>(artifactIds);
    todo.addAll(Arrays.asList("kernel", "stdlib"));

    Set<String> done = new HashSet<String>();
    Set<CheckAppResult> dependencies = new HashSet<CheckAppResult>();
    while (!todo.isEmpty()) {
      String dependency = todo.removeFirst();
      if (!done.contains(dependency)) {
        done.add(dependency);
        CheckAppResult appResult = resolve(dependency, appInfos);
        todo.addAll(appResult.getApplications());
        dependencies.add(appResult);
      }
    }
    return dependencies;
  }
}
