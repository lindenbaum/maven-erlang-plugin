package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.archiver.TarGzUnarchiver;
import eu.lindenbaum.maven.erlang.CheckAppResult;
import eu.lindenbaum.maven.erlang.CheckAppScript;
import eu.lindenbaum.maven.erlang.GetAppupDirectiveScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} will generate an application upgrade file template for an
 * application. In order to do so all available non-SNAPSHOT versions of the
 * project will be resolved (also from remote repositories). All versions
 * smaller than the current version will be included in the generation. The
 * resulting application upgrade file's location will be prompted on success as
 * well as its content.
 * </p>
 * <p>
 * The generation algorithm is outlined in the following description:
 * <ul>
 * <li>Find added modules and add a <code>{load_module, Module}</code>
 * statements for them.</li>
 * <li>Find updated modules and invoke the {@link GetAppupDirectiveScript} on
 * them.</li>
 * <li>Find deleted modules and add a <code>{delete_module, Module}</code>
 * statements for them.</li>
 * </ul>
 * </p>
 * <p>
 * The {@link GetAppupDirectiveScript} works as described below:
 * <ul>
 * <li>Add a <code>{update, Module, supervisor}</code> statement if
 * <code>Module</code> is a supervisor.</li>
 * <li>Add a <code>{update, Module}</code> statement if <code>Module</code>
 * exports <code>code_change/3</code>.</li>
 * <li>Add a <code>{load_module, Module}</code> statement if <code>Module</code>
 * does not meet the previous conditions.</li>
 * </ul>
 * </p>
 * 
 * @goal appup
 * @since 2.1.0
 * @execute phase="package" lifecycle="appup-relup"
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @see <a href="http://www.erlang.org/doc/man/appup.html"
 *      >http://www.erlang.org/doc/man/appup.html</a>
 */
public final class AppupGenerator extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" A P P U P - G E N E R A T O R");
    log.info(MavenUtils.SEPARATOR);

    PackagingType packagingType = p.packagingType();
    if (packagingType == PackagingType.ERLANG_REL) {
      throw new MojoExecutionException("Mojo does not support packaging type " + packagingType + ".");
    }

    Artifact artifact = p.project().getArtifact();
    File projectAppFile = p.targetAppFile();
    TarGzUnarchiver unarchiver = new TarGzUnarchiver(p.node(), p.cookie(), p.target());
    Set<ArtifactVersion> availableVersions = MavenUtils.getAvailableVersions(artifact, p.components());

    Map<String, List<String>> upFromDirectivesMap = new HashMap<String, List<String>>();
    Map<String, List<String>> downToDirectivesMap = new HashMap<String, List<String>>();

    // process earlier non-snapshot versions
    VersionRange range = MavenUtils.createVersionRange("(, " + artifact.getVersion() + ")");
    for (ArtifactVersion artifactVersion : availableVersions) {
      if (range.containsVersion(artifactVersion)) {
        Artifact toResolve = MavenUtils.getArtifact(artifact, artifactVersion.toString(), p.components());
        if (!toResolve.isSnapshot()) {
          extract(unarchiver, MavenUtils.getArtifactFile(toResolve, p.components()));
          String toResolveVersion = toResolve.getBaseVersion();
          String toResolveBase = toResolve.getArtifactId() + "-" + toResolveVersion;
          File toResolveEbin = new File(new File(p.target(), toResolveBase), "ebin");
          File toResolveAppFile = new File(toResolveEbin, artifact.getArtifactId() + ErlConstants.APP_SUFFIX);
          // up from part
          upFromDirectivesMap.put(toResolveVersion, getDirectives(p, toResolveAppFile, projectAppFile));
          // down to part
          downToDirectivesMap.put(toResolveVersion, getDirectives(p, projectAppFile, toResolveAppFile));
        }
      }
    }

    String upFrom = directivesToString(upFromDirectivesMap);
    String downTo = directivesToString(downToDirectivesMap);

    File appupTemplate = new File(p.target(), artifact.getArtifactId() + ErlConstants.APPUP_SUFFIX);
    FileUtils.writeFile(appupTemplate, "{${VERSION},\n " + upFrom + ",\n " + downTo + "}.");
    if (appupTemplate.isFile()) {
      log.info("Successfully generated application upgrade template.");
      MavenUtils.logContent(log, LogLevel.INFO, appupTemplate, "");
    }
    else {
      throw new MojoExecutionException("Failed to create application upgrade file.");
    }
  }

  /**
   * Generates the release upgrade instructions for two versions of an
   * application.
   */
  private List<String> getDirectives(Properties p, File baseAppFile, File targetAppFile) throws MojoExecutionException {
    Script<CheckAppResult> baseScript = new CheckAppScript(baseAppFile);
    CheckAppResult baseAppResult = MavenSelf.get(p.cookie()).exec(p.node(), baseScript);
    List<String> baseModules = baseAppResult.getModules();

    Script<CheckAppResult> targetScript = new CheckAppScript(targetAppFile);
    CheckAppResult targetAppResult = MavenSelf.get(p.cookie()).exec(p.node(), targetScript);
    List<String> targetModules = targetAppResult.getModules();

    ArrayList<String> directives = new ArrayList<String>();

    // process added modules
    List<String> addedModules = new ArrayList<String>(targetModules);
    addedModules.removeAll(baseModules);
    for (String addedModule : addedModules) {
      directives.add("{load_module,'" + addedModule + "'}");
    }

    // process updated modules
    List<String> updatedModules = new ArrayList<String>(targetModules);
    updatedModules.retainAll(baseModules);
    for (String updatedModule : updatedModules) {
      Script<String> appupScript = new GetAppupDirectiveScript(updatedModule, baseAppFile.getParentFile());
      String directive = MavenSelf.get(p.cookie()).exec(p.node(), appupScript);
      if (directive == null) {
        throw new MojoExecutionException("Failed to generate directive for module " + updatedModule + ".");
      }
      directives.add(directive);
    }

    // process deleted modules
    List<String> deletedModules = new ArrayList<String>(baseModules);
    deletedModules.removeAll(targetModules);
    for (String deletedModule : deletedModules) {
      directives.add("{delete_module,'" + deletedModule + "'}");
    }

    return directives;
  }

  /**
   * Returns a string with update/downgrade directives for a spcific list of
   * versions.
   */
  private static String directivesToString(Map<String, List<String>> directives) {
    int i = 0;
    StringBuilder output = new StringBuilder("[");
    for (Entry<String, List<String>> version : directives.entrySet()) {
      if (i++ != 0) {
        output.append(",\n  ");
      }
      String directiveString = ErlUtils.toList(version.getValue(), null, "", "");
      output.append("{\"" + version.getKey() + "\", " + directiveString + "}");
    }
    output.append("]");
    return output.toString();
  }

  /**
   * Wraps extraction of an tar gz archive.
   */
  private static void extract(TarGzUnarchiver unarchiver, File file) throws MojoExecutionException {
    try {
      unarchiver.extract(file);
    }
    catch (IOException e) {
      throw new MojoExecutionException("Failed to extract " + file + ".", e);
    }
  }
}
