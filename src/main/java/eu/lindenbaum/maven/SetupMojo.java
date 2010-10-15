package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.extractFilesFromJar;
import static eu.lindenbaum.maven.util.MavenUtils.getPluginFile;

import java.io.File;
import java.io.IOException;
import java.util.Set;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.plexus.util.FileUtils;

/**
 * <p>
 * Utility goal that will setup a new Erlang/OTP Maven project, creating the
 * basic resources and folders required, from a <em>best practice</em> point of
 * view.
 * </p>
 * <p>
 * This will typically replaces the functionality given by a Maven archetype,
 * checking and creating any missing resource:
 * </p>
 * <ul>
 * <li><code>src/main/erlang</code></li>
 * <li><code>src/main/erlang/[artifactId].app</code></li>
 * <li><code>src/main/include</code></li>
 * <li><code>src/main/priv</code></li>
 * <li><code>src/test/erlang</code></li>
 * <li><code>src/test/include</code></li>
 * <li><code>src/site/site.xml</code></li>
 * <li><code>src/site/apt/index.apt[.vm]</code></li>
 * <li><code>src/changes/changes.xml</code></li>
 * </ul>
 * 
 * @goal setup
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class SetupMojo extends AbstractErlangMojo {

  /**
   * @parameter expression="${basedir}/src/site"
   * @required
   * @readonly
   */
  private File srcSiteDir;

  /**
   * @parameter expression="${basedir}/src/site/apt"
   * @required
   * @readonly
   */
  private File srcSiteAptDir;

  /**
   * @parameter expression="site.xml"
   * @required
   * @readonly
   */
  private String siteXmlFileName;

  /**
   * @parameter expression="${basedir}/src/changes"
   * @required
   * @readonly
   */
  private String srcChangesDir;

  /**
   * @parameter expression="changes.xml"
   * @required
   * @readonly
   */
  private String changesXmlFile;

  private volatile File plugin;

  @Override
  public void execute() throws MojoExecutionException {

    this.plugin = getPluginFile("maven-erlang-plugin", this.project, this.repository);

    if (noSite()) {
      createDefaultSite();
    }
    else {
      logSkipping();
    }

    if (noSiteIndexApt()) {
      createDefaultSiteIndexApt();
    }
    else {
      logSkipping();
    }

    if (noChangelog()) {
      createDefaultChangelog();
    }
    else {
      logSkipping();
    }

    if (noSrcMainErlang()) {
      createSrcMainErlang();
    }
    else {
      logSkipping();
    }

    if (noSrcMainInclude()) {
      createSrcMainInclude();
    }
    else {
      logSkipping();
    }

    if (noSrcMainPriv()) {
      createSrcMainPriv();
    }
    else {
      logSkipping();
    }

    if (noSrcTestErlang()) {
      createSrcTestErlang();
    }
    else {
      logSkipping();
    }

    if (noSrcTestInclude()) {
      createSrcTestInclude();
    }
    else {
      logSkipping();
    }

    if (noSrcMainErlangAppFile()) {
      createSrcMainErlangAppFile();
    }
    else {
      logSkipping();
    }

    getLog().info("-->");
    getLog().info("--> Checking dependencies...");
    checkIfProjectHasDependentArtifact("maven-changes-plugin");
    getLog().info("--> Done!");
  }

  private void checkIfProjectHasDependentArtifact(String artifactId) {
    Artifact resolved = findProjectDependencyArtifact(artifactId);
    if (resolved == null) {
      getLog().info("--> MISSING REQUIRED ARTIFACT: [" + artifactId + "]");
    }
  }

  @SuppressWarnings("unchecked")
  private Artifact findProjectDependencyArtifact(String artifactId) {
    Set<Artifact> plugins = this.project.getPluginArtifacts();
    if (plugins != null) {
      for (Artifact artifact : plugins) {
        if (artifact.getArtifactId().equals(artifactId)) {
          return artifact;
        }
      }
    }
    Set<Artifact> dependencies = this.project.getDependencyArtifacts();
    if (dependencies != null) {
      for (Artifact artifact : dependencies) {
        if (artifact.getArtifactId().equals(artifactId)) {
          return artifact;
        }
      }
    }
    return null;
  }

  private boolean noSiteIndexApt() {
    boolean aptExists = new File(this.srcSiteAptDir, "index.apt").isFile();
    boolean vmAptExists = new File(this.srcSiteAptDir, "index.apt.vm").isFile();
    logCheckingIf("site index.apt[.vm] file", aptExists || vmAptExists);
    return !aptExists && !vmAptExists;
  }

  private void createDefaultSiteIndexApt() throws MojoExecutionException {
    logGeneratingDefault("site index.apt.vm file");
    extractFilesFromJar(this.plugin, "index.apt.vm", this.srcSiteAptDir);
  }

  private boolean noSrcMainErlangAppFile() {
    boolean exists = new File(this.srcMainErlang, this.project.getArtifactId() + ErlConstants.APP_SUFFIX).isFile();
    logCheckingIf("app file", exists);
    return !exists;
  }

  private void createSrcMainErlangAppFile() throws MojoExecutionException {
    logGeneratingDefault("app file");
    File targetFile = new File(this.srcMainErlang, this.project.getArtifactId() + ErlConstants.APP_SUFFIX);
    String defaultAppFileName = "default.app";
    extractFilesFromJar(this.plugin, defaultAppFileName, this.srcMainErlang);
    File sourceFile = new File(this.srcMainErlang, defaultAppFileName);
    try {
      FileUtils.copyFile(sourceFile, targetFile);
    }
    catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
    sourceFile.delete();
  }

  private boolean noSrcTestInclude() {
    boolean exists = this.srcTestInclude.isDirectory();
    logCheckingIf("source test include", exists);
    return !exists;
  }

  private void createSrcTestInclude() {
    logGeneratingDefault("source test include");
    this.srcTestInclude.mkdirs();
  }

  private boolean noSrcTestErlang() {
    boolean exists = this.srcTestErlang.isDirectory();
    logCheckingIf("source test erlang", exists);
    return !exists;
  }

  private void createSrcTestErlang() {
    logGeneratingDefault("source test erlang");
    this.srcTestErlang.mkdirs();
  }

  private boolean noSrcMainPriv() {
    boolean exists = this.srcMainPriv.isDirectory();
    logCheckingIf("source priv folder", exists);
    return !exists;
  }

  private void createSrcMainPriv() {
    logGeneratingDefault("source priv folder");
    this.srcMainPriv.mkdirs();
  }

  private boolean noSrcMainInclude() {
    boolean exists = this.srcMainInclude.isDirectory();
    logCheckingIf("source main include", exists);
    return !exists;
  }

  private void createSrcMainInclude() {
    logGeneratingDefault("source main include");
    this.srcMainInclude.mkdirs();
  }

  private boolean noSrcMainErlang() {
    boolean exists = this.srcMainErlang.isDirectory();
    logCheckingIf("source main erlang", exists);
    return !exists;
  }

  private void createSrcMainErlang() {
    this.srcMainErlang.mkdirs();
    logGeneratingDefault("source main erlang");
  }

  private boolean noSite() {
    boolean exists = new File(this.srcSiteDir, this.siteXmlFileName).isFile();
    logCheckingIf("site section", exists);
    return !exists;
  }

  private void createDefaultSite() throws MojoExecutionException {
    logGeneratingDefault("site section");
    extractFilesFromJar(this.plugin, this.siteXmlFileName, this.srcSiteDir);
  }

  private boolean noChangelog() {
    boolean exists = new File(this.srcChangesDir, this.changesXmlFile).isFile();
    logCheckingIf("changelog", exists);
    return !exists;
  }

  private void createDefaultChangelog() throws MojoExecutionException {
    logGeneratingDefault("changelog");
    File targetDir = new File(this.srcChangesDir);
    extractFilesFromJar(this.plugin, this.changesXmlFile, targetDir);
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
