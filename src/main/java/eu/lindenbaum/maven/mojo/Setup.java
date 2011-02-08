package eu.lindenbaum.maven.mojo;

import static eu.lindenbaum.maven.util.FileUtils.extractFilesFromJar;
import static eu.lindenbaum.maven.util.MavenUtils.getPluginFile;

import java.io.File;
import java.io.IOException;
import java.util.Set;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.plexus.util.FileUtils;

/**
 * <p>
 * Utility goal that will setup a new Erlang/OTP Maven project, creating the
 * basic resources and folders required, from a <em>best practice</em> point of
 * view.
 * </p>
 * <p>
 * This will typically replace the functionality given by a Maven archetype,
 * checking and creating any missing resource:
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
 * <li>site folder with <code>site.xml</code></li>
 * <li>apt source folder with <code>index.apt[.vm]</code></li>
 * <li>changelog folder with <code>changes.xml</code></li>
 * </ul>
 * <p>
 * The default application resource file will look like:
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
 * {${VERSION}, [], []}.
 * </pre>
 * </p>
 * 
 * @goal setup
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class Setup extends ErlangMojo {
  /**
   * @parameter expression="site.xml"
   * @required
   * @readonly
   */
  private String siteXmlFileName;

  /**
   * @parameter expression="changes.xml"
   * @required
   * @readonly
   */
  private String changesXmlFile;

  private volatile File plugin;

  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    PackagingType packagingType = p.packagingType();
    if (packagingType != PackagingType.ERLANG_OTP && packagingType != PackagingType.ERLANG_STD) {
      log.info("--> There's nothing to do for packaging " + packagingType + ".");
      log.info("--> Done!");
      return;
    }

    this.plugin = getPluginFile("maven-erlang-plugin", p.project(), p.repository());

    if (noSite(p)) {
      createDefaultSite(p);
    }
    else {
      logSkipping();
    }

    if (noApt(p)) {
      createDefaultIndexApt(p);
    }
    else {
      logSkipping();
    }

    if (noChangelog(p)) {
      createDefaultChangelog(p);
    }
    else {
      logSkipping();
    }

    if (noSourceFolder(p)) {
      createSourceFolder(p);
    }
    else {
      logSkipping();
    }

    if (noIncludeFolder(p)) {
      createIncludeFolder(p);
    }
    else {
      logSkipping();
    }

    if (noPrivFolder(p)) {
      createPrivFolder(p);
    }
    else {
      logSkipping();
    }

    if (noTestSourceFolder(p)) {
      createTestSourceFolder(p);
    }
    else {
      logSkipping();
    }

    if (noTestIncludeFolder(p)) {
      createTestIncludeFolder(p);
    }
    else {
      logSkipping();
    }

    if (noTestPrivFolder(p)) {
      createTestPrivFolder(p);
    }
    else {
      logSkipping();
    }

    if (noEbinFolder(p)) {
      createEbinFolder(p);
    }
    else {
      logSkipping();
    }

    if (noAppFile(p)) {
      createAppFile(p);
    }
    else {
      logSkipping();
    }

    if (noAppUpFile(p)) {
      createAppUpFile(p);
    }
    else {
      logSkipping();
    }

    log.info("-->");
    log.info("--> Checking dependencies...");
    checkIfProjectHasDependentArtifact(p, "maven-changes-plugin");
    log.info("--> Done!");
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

  private boolean noApt(Properties p) {
    boolean aptExists = new File(p.apt(), "index.apt").isFile();
    boolean vmAptExists = new File(p.apt(), "index.apt.vm").isFile();
    logCheckingIf("site index.apt[.vm] file", aptExists || vmAptExists);
    return !aptExists && !vmAptExists;
  }

  private void createDefaultIndexApt(Properties p) throws MojoExecutionException {
    logGeneratingDefault("site index.apt.vm file");
    extractFilesFromJar(this.plugin, "index.apt.vm", p.apt());
  }

  private boolean noAppFile(Properties p) {
    boolean exists = new File(p.ebin(), p.project().getArtifactId() + ErlConstants.APP_SUFFIX).isFile();
    logCheckingIf("app file", exists);
    return !exists;
  }

  private void createAppFile(Properties p) throws MojoExecutionException {
    logGeneratingDefault("app file");
    File targetFile = new File(p.ebin(), p.project().getArtifactId() + ErlConstants.APP_SUFFIX);
    String defaultAppFileName = "default.app";
    extractFilesFromJar(this.plugin, defaultAppFileName, p.ebin());
    File sourceFile = new File(p.ebin(), defaultAppFileName);
    try {
      FileUtils.copyFile(sourceFile, targetFile);
    }
    catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
    sourceFile.delete();
  }

  private boolean noAppUpFile(Properties p) {
    boolean exists = new File(p.ebin(), p.project().getArtifactId() + ErlConstants.APPUP_SUFFIX).isFile();
    logCheckingIf("appup file", exists);
    return !exists;
  }

  private void createAppUpFile(Properties p) throws MojoExecutionException {
    logGeneratingDefault("appup file");
    File targetFile = new File(p.ebin(), p.project().getArtifactId() + ErlConstants.APPUP_SUFFIX);
    String defaultAppUpFileName = "default.appup";
    extractFilesFromJar(this.plugin, defaultAppUpFileName, p.ebin());
    File sourceFile = new File(p.ebin(), defaultAppUpFileName);
    try {
      FileUtils.copyFile(sourceFile, targetFile);
    }
    catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
    sourceFile.delete();
  }

  private boolean noTestIncludeFolder(Properties p) {
    boolean exists = p.test_include().isDirectory();
    logCheckingIf("test include folder", exists);
    return !exists;
  }

  private void createTestIncludeFolder(Properties p) {
    logGeneratingDefault("test include folder");
    p.test_include().mkdirs();
  }

  private boolean noTestSourceFolder(Properties p) {
    boolean exists = p.test_src().isDirectory();
    logCheckingIf("test source folder", exists);
    return !exists;
  }

  private void createTestSourceFolder(Properties p) {
    logGeneratingDefault("test source folder");
    p.test_src().mkdirs();
  }

  private boolean noTestPrivFolder(Properties p) {
    boolean exists = p.test_priv().isDirectory();
    logCheckingIf("test priv folder", exists);
    return !exists;
  }

  private void createTestPrivFolder(Properties p) {
    logGeneratingDefault("test priv folder");
    p.test_priv().mkdirs();
  }

  private boolean noPrivFolder(Properties p) {
    boolean exists = p.priv().isDirectory();
    logCheckingIf("priv folder", exists);
    return !exists;
  }

  private void createPrivFolder(Properties p) {
    logGeneratingDefault("priv folder");
    p.priv().mkdirs();
  }

  private boolean noIncludeFolder(Properties p) {
    boolean exists = p.include().isDirectory();
    logCheckingIf("include folder", exists);
    return !exists;
  }

  private void createIncludeFolder(Properties p) {
    logGeneratingDefault("include folder");
    p.include().mkdirs();
  }

  private boolean noSourceFolder(Properties p) {
    boolean exists = p.src().isDirectory();
    logCheckingIf("source folder", exists);
    return !exists;
  }

  private void createSourceFolder(Properties p) {
    p.src().mkdirs();
    logGeneratingDefault("source folder");
  }

  private boolean noEbinFolder(Properties p) {
    boolean exists = p.ebin().isDirectory();
    logCheckingIf("application resource folder", exists);
    return !exists;
  }

  private void createEbinFolder(Properties p) {
    p.ebin().mkdirs();
    logGeneratingDefault("application resource folder");
  }

  private boolean noSite(Properties p) {
    boolean exists = new File(p.site(), this.siteXmlFileName).isFile();
    logCheckingIf("site section", exists);
    return !exists;
  }

  private void createDefaultSite(Properties p) throws MojoExecutionException {
    logGeneratingDefault("site section");
    extractFilesFromJar(this.plugin, this.siteXmlFileName, p.site());
  }

  private boolean noChangelog(Properties p) {
    boolean exists = new File(p.changes(), this.changesXmlFile).isFile();
    logCheckingIf("changelog", exists);
    return !exists;
  }

  private void createDefaultChangelog(Properties p) throws MojoExecutionException {
    logGeneratingDefault("changelog");
    extractFilesFromJar(this.plugin, this.changesXmlFile, p.changes());
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
