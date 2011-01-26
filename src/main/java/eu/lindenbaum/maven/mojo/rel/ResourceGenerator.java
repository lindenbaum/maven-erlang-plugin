package eu.lindenbaum.maven.mojo.rel;

import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
 * dependencies application and version tuples</li>
 * <li><code>${AUTODEPS}</code>: an erlang list with all dependency applications
 * of the project including the standard applications <code>kernel</code>,
 * <code>stdlib</code> and the applications provided in the
 * {@link #additionalApplications} parameter</li>
 * <li><code>${<i>APPLICATION_NAME</i>}</code>: will be replaced by a string
 * representing the available application version on this host</li>
 * </ul>
 * 
 * @goal generate-release-resources
 * @phase generate-resources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Greg Haskins <ghaskins@novell.com>
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

  /**
   * Additional applications that are dependencies for this release (e.g.
   * <code>mnesia</code>). By default application dependencies are
   * <code>kernel</code> and <code>stdlib</code>. These must not be added to
   * this additional list.
   * 
   * @parameter expression="${additionalDependencies}"
   */
  private String[] additionalApplications;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    RuntimeInfoScript infoScript = new RuntimeInfoScript();
    RuntimeInfo runtimeInfo = MavenSelf.get(p.cookie()).exec(p.node(), infoScript, new ArrayList<File>());

    List<Artifact> artifacts = MavenUtils.getErlangReleaseArtifacts(p.project());
    List<Artifact> otpArtifacts = getOtpArtifacts(p, runtimeInfo.getLibDirectory());

    String releaseName = p.project().getArtifactId();
    String releaseVersion = p.project().getVersion();
    String relFileBaseName = releaseName + "-" + releaseVersion;

    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("${ARTIFACT}", "\"" + releaseName + "\"");
    replacements.put("${VERSION}", "\"" + releaseVersion + "\"");
    replacements.put("${ERTS}", "\"" + runtimeInfo.getVersion() + "\"");
    replacements.put("${APPLICATIONS}", getReleaseDependencies(artifacts));

    List<Artifact> autoDeps = new ArrayList<Artifact>(artifacts);
    autoDeps.addAll(filter(otpArtifacts, Arrays.asList("kernel", "stdlib")));
    if (this.additionalApplications != null) {
      autoDeps.addAll(filter(otpArtifacts, Arrays.asList(this.additionalApplications)));
    }
    replacements.put("${AUTODEPS}", "[" + getReleaseDependencies(autoDeps) + "]");

    List<Artifact> allApplications = new ArrayList<Artifact>(otpArtifacts);
    allApplications.addAll(artifacts);
    replacements.putAll(getApplicationMappings(allApplications));

    int resources = copyDirectory(p.ebin(), p.target(), FileUtils.REL_FILTER, replacements);
    log.debug("Copied " + resources + " release files.");

    File relFile = new File(p.target(), relFileBaseName + ErlConstants.REL_SUFFIX);
    if (!relFile.isFile()) {
      throw new MojoFailureException("Could not find release file " + relFile.getName() + ".");
    }

    List<File> codePaths = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    codePaths.add(p.target());

    Script<SystoolsScriptResult> script = new MakeScriptScript(relFile, p.target(), this.scriptOptions);
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
  private static Map<String, String> getApplicationMappings(List<Artifact> artifacts) {
    HashMap<String, String> mappings = new HashMap<String, String>();
    for (Artifact artifact : artifacts) {
      String key = "${" + artifact.getArtifactId().toUpperCase() + "}";
      mappings.put(key, "\"" + artifact.getVersion() + "\"");
    }
    return mappings;
  }

  /**
   * Returns a {@link Map} containing version mappings for all applications
   * available in the local OTP installation.
   */
  private static List<Artifact> getOtpArtifacts(Properties p, File libDirectory) throws MojoExecutionException {
    List<Artifact> artifacts = new ArrayList<Artifact>();
    for (File appFile : FileUtils.getFilesRecursive(libDirectory, ErlConstants.APP_SUFFIX)) {
      Script<CheckAppResult> script = new CheckAppScript(appFile);
      CheckAppResult result = MavenSelf.get(p.cookie()).exec(p.node(), script, new ArrayList<File>());
      artifacts.add(MavenUtils.getArtifact(result.getName(), result.getVersion()));
    }
    return artifacts;
  }

  /**
   * Returns a {@link List} filtered for the {@link Artifact}s contained int the
   * list of artifactIds.
   */
  private static List<Artifact> filter(List<Artifact> artifacts, List<String> artifactIds) {
    List<Artifact> result = new ArrayList<Artifact>();
    for (Artifact artifact : artifacts) {
      if (artifactIds.contains(artifact.getArtifactId())) {
        result.add(artifact);
      }
    }
    return result;
  }

  /**
   * Returns a comma separated string of application version tuples taken from
   * the projects dependency section. Result string will look like
   * <code>{"app1", "version1"}, {"app2", "version2"}, ...</code>.
   */
  private static String getReleaseDependencies(List<Artifact> artifacts) {
    StringBuilder applications = new StringBuilder();
    for (int i = 0; i < artifacts.size(); ++i) {
      if (i != 0) {
        applications.append(",\n  ");
      }
      Artifact artifact = artifacts.get(i);
      applications.append("{\'");
      applications.append(artifact.getArtifactId());
      applications.append("\', \"");
      applications.append(artifact.getVersion());
      applications.append("\"}");
    }
    return applications.toString();
  }
}
