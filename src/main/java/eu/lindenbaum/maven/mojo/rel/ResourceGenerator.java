package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
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
import eu.lindenbaum.maven.erlang.GenericScriptResult;
import eu.lindenbaum.maven.erlang.MakeScriptScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.RuntimeInfo;
import eu.lindenbaum.maven.erlang.RuntimeInfoScript;
import eu.lindenbaum.maven.erlang.Script;
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
 * <li>release upgrade file (relup)</li>
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
 * <li><code>${ERTS}</code>: expands to the tuple
 * <code>{erts, "ERTS_VERSION"}</code> with the version of erts available on the
 * backend node (tuple)</li>
 * <li><code>${APPLICATIONS}</code>: a comma separated listing with all
 * transitive dependency applications of the project (tuple listing)</li>
 * <li><code>${AUTODEPS}</code>: an erlang list with all transitive dependency
 * applications of the project (list)</li>
 * <li><code>${<i>APPLICATION_NAME</i>}</code>: will be replaced by the tuple
 * <code>{'APPLICATION_NAME', "APPLICATION_VERSION"}</code> with the version
 * available on the backend node (tuple)</li>
 * </ul>
 * 
 * @goal generate-release-resources
 * @phase generate-resources
 * @requiresDependencyResolution test
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
    RuntimeInfo runtimeInfo = MavenSelf.get(p.cookie()).exec(p.node(), infoScript);

    String releaseName = p.project().getArtifactId();
    String releaseVersion = p.project().getVersion();

    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("${ARTIFACT}", "\"" + releaseName + "\"");
    replacements.put("${VERSION}", "\"" + releaseVersion + "\"");
    replacements.put("${ERTS}", "{erts, \"" + runtimeInfo.getVersion() + "\"}");

    Set<Artifact> artifacts = MavenUtils.getErlangReleaseArtifacts(p.project());
    Set<String> artifactIds = MavenUtils.getArtifactIds(artifacts);
    artifactIds.addAll(Arrays.asList("kernel", "stdlib", "sasl"));

    File base = p.targetLayout().base();

    List<File> codePaths = p.codePaths(false);
    codePaths.addAll(runtimeInfo.getPaths());
    codePaths.removeAll(Arrays.asList(base));
    Collections.reverse(codePaths);

    log.debug("Application lookup path is: " + codePaths);

    Map<String, CheckAppResult> appInfos = getAppInfos(p, codePaths);
    Set<CheckAppResult> autoDependencies = getDependencies(artifactIds, appInfos);

    log.debug("Found dependencies: " + autoDependencies);

    String applicationTuples = ErlUtils.toApplicationTuples(autoDependencies.toArray(new CheckAppResult[0]));
    replacements.put("${APPLICATIONS}", applicationTuples);
    replacements.put("${AUTODEPS}", "[" + applicationTuples + "]");
    replacements.putAll(getApplicationMappings(appInfos));

    log.debug("Created mappings: " + replacements);

    File relFile = p.targetLayout().relFile();
    checkReleaseFile(log, p.sourceLayout().relFile());
    FileUtils.copyFile(p.sourceLayout().relFile(), relFile, replacements);
    log.debug("Copied release file to " + relFile + " .");

    File relupFile = p.targetLayout().relupFile();
    checkReleaseUpgradeFile(log, p.sourceLayout().relupFile());
    FileUtils.copyFile(p.sourceLayout().relupFile(), relupFile, replacements);
    log.debug("Copied release upgrade file to " + relupFile + " .");

    File sysConfigFile = p.targetLayout().sysConfigFile();
    checkSystemConfig(log, p.sourceLayout().sysConfigFile());
    FileUtils.copyFile(p.sourceLayout().sysConfigFile(), sysConfigFile, replacements);
    log.debug("Copied system configuration file to " + sysConfigFile + " .");

    Script<GenericScriptResult> script = new MakeScriptScript(relFile, base, this.scriptOptions);
    GenericScriptResult makeScriptResult = MavenSelf.get(p.cookie()).exec(p.node(), script);
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
      mappings.put(key, "{'" + appResult.getName() + "',\"" + appResult.getVersion() + "\"}");
    }
    return mappings;
  }

  /**
   * Returns a {@link Map} containing {@link CheckAppResult} mappings for all
   * applications available in the given lib directories installation.
   */
  private static Map<String, CheckAppResult> getAppInfos(Properties p, List<File> paths) throws MojoExecutionException {
    Map<String, CheckAppResult> applications = new HashMap<String, CheckAppResult>();
    for (File path : paths) {
      for (File appFile : FileUtils.getFilesRecursive(path, ErlConstants.APP_SUFFIX)) {
        Script<CheckAppResult> script = new CheckAppScript(appFile);
        CheckAppResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
        applications.put(result.getName(), result);
      }
    }
    return applications;
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
    Set<String> done = new HashSet<String>();
    LinkedList<String> todo = new LinkedList<String>(artifactIds);
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

  /**
   * Checks whether the vital system configuration {@code sys.config} exists.
   */
  private static void checkSystemConfig(Log log, File sysConfig) throws MojoFailureException {
    if (!sysConfig.isFile()) {
      log.error("Errors:");
      log.error(" * system configuration file not found, use 'mvn erlang:setup' to create");
      log.error("   a default system configuration file");
      throw new MojoFailureException(sysConfig.toString() + " does not exist.");
    }
  }

  /**
   * Checks whether the vital release upgrade file {@code relup} exists.
   */
  private static void checkReleaseUpgradeFile(Log log, File relup) throws MojoFailureException {
    if (!relup.isFile()) {
      log.error("Errors:");
      log.error(" * release upgrade file not found, use 'mvn erlang:relup' or");
      log.error("   'mvn erlang:setup' to create a template relup file");
      throw new MojoFailureException(relup.toString() + " does not exist.");
    }
  }

  /**
   * Checks whether the vital release upgrade file {@code relup} exists.
   */
  private static void checkReleaseFile(Log log, File rel) throws MojoFailureException {
    if (!rel.isFile()) {
      log.error("Errors:");
      log.error(" * release file not found, use 'mvn erlang:setup' to create a default");
      log.error("   release file");
      throw new MojoFailureException(rel.toString() + " does not exist.");
    }
  }
}
