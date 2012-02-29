package eu.lindenbaum.maven.mojo;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.SourceLayout;
import eu.lindenbaum.maven.util.FileUtils;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * Utility goal that will setup a new Erlang/OTP Maven project, creating the
 * basic resources and folders required, from a <em>best practice</em> point of
 * view.
 * </p>
 * <p>
 * This will typically replace the functionality given by a Maven archetype,
 * checking and creating any missing resource. Regarding Erlang/OTP applications
 * this could be:
 * </p>
 * <ul>
 * <li>source folder</li>
 * <li>application resource file <code>[artifactId].app</code></li>
 * <li>application upgrade file <code>[artifactId].appup</code></li>
 * <li>include folder</li>
 * <li>priv folder</li>
 * <li>test source folder</li>
 * <li>test include folder</li>
 * <li>test priv folder</li>
 * </ul>
 * <p>
 * Regarding Erlang/OTP releases this could be:
 * </p>
 * <ul>
 * <li>release file <code>[artifactId].rel</code></li>
 * <li>system configuration <code>sys.config</code></li>
 * </ul>
 * <p>
 * By default some extras for project management are also generated:
 * </p>
 * <ul>
 * <li>site folder with <code>site.xml</code></li>
 * <li>apt source folder with <code>index.apt[.vm]</code></li>
 * <li>changelog folder with <code>changes.xml</code></li>
 * </ul>
 * <p>
 * The default application resource file will look like (this would be
 * sufficient for library applications):
 * 
 * <pre>
 * {application, ${ARTIFACT},
 *  [{description, ${DESCRIPTION}},
 *   {id, ${ID}},
 *    {vsn, ${VERSION}},
 *    {modules, ${MODULES}},
 *    {maxT, infinity},
 *    {registered, ${REGISTERED}},
 *    {included_applications, []},
 *    {applications, [kernel, stdlib]},
 *    {env, []}, 
 *    {start_phases, []}]}.
 * </pre>
 * </p>
 * <p>
 * The default application upgrade file will look like:
 * 
 * <pre>
 * {${VERSION}, [EDIT HERE], [EDIT HERE]}.
 * </pre>
 * </p>
 * <p>
 * The default release file will look like:
 * 
 * <pre>
 * {release,
 *  {${ARTIFACT}, ${VERSION}},
 *  ${ERTS},
 *  ${AUTODEPS}}.
 * </pre>
 * </p>
 * <p>
 * The default release upgrade file will look like:
 * 
 * <pre>
 * {${VERSION}, [EDIT HERE], [EDIT HERE]}.
 * </pre>
 * </p>
 * <p>
 * The default system configuration file will look like:
 * 
 * <pre>
 * [].
 * </pre>
 * </p>
 * 
 * @goal setup
 * @requiresDependencyResolution test
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class Setup extends ErlangMojo {
  /**
   * Setting this to {@code false} will skip generation of the project resources
   * considered best-practice <em>extras</em> - such as Maven site definition
   * XML document, an index APT page and a changes XML document for the
   * maven-changes-plugin. It is of course encouraged to leave this as defined
   * by default.
   * 
   * @parameter expression="${withExtras}" default-value=true
   * @since 2.0.0
   */
  private boolean withExtras;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    PackagingType packagingType = p.packagingType();
    if (packagingType == PackagingType.ERLANG_OTP || packagingType == PackagingType.ERLANG_STD) {
      setupAppDefaults(p);
    }
    else {
      setupRelDefaults(p);
    }
    log.info("-->");

    if (this.withExtras) {
      setupExtras(p);
      log.info("--> Checking dependencies...");
      checkIfProjectHasDependentArtifact(p, "maven-changes-plugin");
    }

    log.info("--> Done!");
  }

  private void setupExtras(Properties p) throws MojoExecutionException {
    // setup extra folders
    Map<String, File> folders = new HashMap<String, File>();
    File site = new File(p.sourceLayout().base(), "src/site");
    File changes = new File(p.sourceLayout().base(), "src/changes");
    File apt = new File(site, "apt");
    folders.put("site folder", site);
    folders.put("changes folder", changes);
    folders.put("apt folder", apt);
    for (Entry<String, File> folder : folders.entrySet()) {
      if (noFolder(folder.getValue(), folder.getKey())) {
        createFolder(folder.getValue(), folder.getKey());
      }
      else {
        logSkipping();
      }
    }

    // setup extra files
    Map<String, File> files = new HashMap<String, File>();
    files.put("default-site.xml", new File(site, "site.xml"));
    files.put("default-changes.xml", new File(changes, "changes.xml"));
    for (Entry<String, File> entry : files.entrySet()) {
      if (noFile(entry.getValue())) {
        createFile(entry.getValue(), entry.getKey());
      }
      else {
        logSkipping();
      }
    }

    File defaultApt = new File(apt, "index.apt.vm");
    if (noFile(new File(apt, "index.apt")) && noFile(defaultApt)) {
      createFile(defaultApt, "default-index.apt.vm");
    }
    else {
      logSkipping();
    }
  }

  private void setupAppDefaults(Properties p) throws MojoExecutionException {
    // setup folders
    SourceLayout sourceLayout = p.sourceLayout();
    Map<String, File> folders = new HashMap<String, File>();
    folders.put("ebin folder", sourceLayout.ebin());
    folders.put("src folder", sourceLayout.src());
    folders.put("include folder", sourceLayout.include());
    folders.put("priv folder", sourceLayout.priv());
    for (File folder : sourceLayout.testSrcs()) {
      folders.put("test src folder", folder);
    }
    folders.put("test include folder", sourceLayout.testInclude());
    folders.put("test priv folder", sourceLayout.testPriv());
    for (Entry<String, File> folder : folders.entrySet()) {
      if (noFolder(folder.getValue(), folder.getKey())) {
        createFolder(folder.getValue(), folder.getKey());
      }
      else {
        logSkipping();
      }
    }

    // setup files
    Map<String, File> files = new HashMap<String, File>();
    files.put("default.app", sourceLayout.appFile());
    files.put("default.appup", sourceLayout.appupFile());
    for (Entry<String, File> entry : files.entrySet()) {
      if (noFile(entry.getValue())) {
        createFile(entry.getValue(), entry.getKey());
      }
      else {
        logSkipping();
      }
    }
  }

  private void setupRelDefaults(Properties p) throws MojoExecutionException {
    // setup files
    SourceLayout sourceLayout = p.sourceLayout();
    Map<String, File> files = new HashMap<String, File>();
    files.put("default.rel", sourceLayout.relFile());
    files.put("default-sys.config", sourceLayout.sysConfigFile());
    files.put("default.relup", sourceLayout.relupFile());
    for (Entry<String, File> entry : files.entrySet()) {
      if (noFile(entry.getValue())) {
        createFile(entry.getValue(), entry.getKey());
      }
      else {
        logSkipping();
      }
    }
  }

  private void checkIfProjectHasDependentArtifact(Properties p, String artifactId) {
    Artifact resolved = findProjectDependencyArtifact(p, artifactId);
    if (resolved == null) {
      getLog().info("--> MISSING REQUIRED ARTIFACT: [" + artifactId + "]");
    }
  }

  @SuppressWarnings("unchecked")
  private Artifact findProjectDependencyArtifact(Properties p, String artifactId) {
    Set<Artifact> plugins = p.project().getPluginArtifacts();
    if (plugins != null) {
      for (Artifact artifact : plugins) {
        if (artifact.getArtifactId().equals(artifactId)) {
          return artifact;
        }
      }
    }
    Set<Artifact> dependencies = p.project().getDependencyArtifacts();
    if (dependencies != null) {
      for (Artifact artifact : dependencies) {
        if (artifact.getArtifactId().equals(artifactId)) {
          return artifact;
        }
      }
    }
    return null;
  }

  private boolean noFolder(File folder, String name) {
    boolean exists = folder.isDirectory();
    logCheckingIf(name, exists);
    return !exists;
  }

  private void createFolder(File folder, String name) throws MojoExecutionException {
    logGeneratingDefault(name);
    FileUtils.ensureDirectories(folder);
  }

  private boolean noFile(File file) {
    boolean exists = file.isFile();
    logCheckingIf(file.getName(), exists);
    return !exists;
  }

  private void createFile(File file, String defaultFileName) throws MojoExecutionException {
    logGeneratingDefault(file.getName());
    FileUtils.extractFileFromClassPath(getClass(), "", defaultFileName, file);
  }

  private void logCheckingIf(String string, boolean exists) {
    getLog().info("--> Checking if " + string + " exists... [" + (exists ? "YES" : "NO") + "]");
  }

  private void logSkipping() {
    getLog().info("--> Skipping generation.");
  }

  private void logGeneratingDefault(String string) {
    getLog().info("--> Generating default " + string + ".");
  }
}
