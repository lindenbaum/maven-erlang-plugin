package eu.lindenbaum.maven;

import java.io.File;

import org.apache.maven.project.MavenProject;

/**
 * Represents a bean interface holding all values the plugin needs to work.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface Properties {
  /**
   * Returns the {@link MavenProject} to process.
   */
  public MavenProject project();

  /**
   * Returns the {@link MavenComponents} that are available to the plugin.
   */
  public MavenComponents components();

  /**
   * Returns the name of the projects build artifact. Usually this is
   * {@code "artifactId-version"}.
   */
  public String projectName();

  /**
   * Returns the file packaged as build artifact for a this project.
   */
  public File projectArtifactFile();

  /**
   * Returns the packaging type of the project artifact.
   */
  public PackagingType packagingType();

  /**
   * Returns the erlang command used to start new backend node's. This may be an
   * absolute or relative path as well as a simple command (assuming the host
   * environment is set up properly). The path must not contain any arguments.
   */
  public String erlCommand();

  /**
   * Returns the name of the backend node to use.
   */
  public String node();

  /**
   * Returns the cookie that must be used when connecting to the backend node.
   */
  public String cookie();

  /**
   * Returns the name of the test backend node to use.
   */
  public String testNode();

  /**
   * Returns the directory where the maven .apt resources reside.
   */
  public File apt();

  /**
   * Returns the base project directory.
   */
  public File base();

  /**
   * Returns the directory where the changes resources reside.
   */
  public File changes();

  /**
   * Returns the directory where the application (upgrade) files reside.
   */
  public File ebin();

  /**
   * Returns the directory where the header files reside.
   */
  public File include();

  /**
   * Returns the directory where the private resources reside.
   */
  public File priv();

  /**
   * Returns the directory where the maven site resources reside.
   */
  public File site();

  /**
   * Returns the directory where the erlang sources reside.
   */
  public File src();

  /**
   * Returns the base folder for sources of this project. This may be used to
   * include sources from other languages into the erlang application.
   */
  public File src_base();

  /**
   * Returns the directory where the erlang test include files reside.
   */
  public File test_include();

  /**
   * Returns the directory where private test resources reside.
   */
  public File test_priv();

  /**
   * Returns the directory where the erlang test source files reside.
   */
  public File test_src();

  /**
   * Returns the application resource file of an application project. The
   * returned {@link File} is not guaranteed to exist.
   */
  public File appFile();

  /**
   * Returns the application upgrade file of an application project. The
   * returned {@link File} is not guaranteed to exist.
   */
  public File appupFile();

  /**
   * Returns the release file of a release project. The returned {@link File} is
   * not guaranteed to exist.
   */
  public File relFile();

  /**
   * Returns the release upgrade file of a release project. The returned
   * {@link File} is not guaranteed to exist.
   */
  public File relupFile();

  /**
   * Returns the system configuration file of a release project. The returned
   * {@link File} is not guaranteed to exist.
   */
  public File sysConfigFile();

  /**
   * Returns the base directory for the build artifacts.
   */
  public File target();

  /**
   * Returns the directory where the compiled sources will be placed into.
   */
  public File targetEbin();

  /**
   * Returns the directory where includes to package will be put into.
   */
  public File targetInclude();

  /**
   * Returns the directories where dependencies get unpacked into.
   */
  public File targetLib();

  /**
   * Returns the directory where SNMP related resources will be put into.
   */
  public File targetMibs();

  /**
   * Returns the directory where private resources will be put into.
   */
  public File targetPriv();

  /**
   * Returns the base directory for the project packaging.
   */
  public File targetProject();

  /**
   * Returns the directoriy where all releases will be put into.
   */
  public File targetReleases();

  /**
   * Returns the directory where site documentation will be generated into.
   */
  public File targetSite();

  /**
   * Returns the directory where sources to package will be put into.
   */
  public File targetSrc();

  /**
   * Returns the directory where the surefire reports will be put into.
   */
  public File targetSurefireReports();

  /**
   * Returns the base directory for the tests. This will be accessibly during
   * the test phase by calling <code>code:lib_dir($APPNAME)</code>.
   */
  public File targetTest();

  /**
   * Returns the directory where the compiled test sources and recompiled
   * sources will be placed into.
   */
  public File targetTestEbin();

  /**
   * Returns the directory where the main and test resources will be put into
   * (test resources will override main resources). This will be accessibly
   * during the test phase by calling <code>code:lib_dir($APPNAME, priv)</code>.
   */
  public File targetTestPriv();

  /**
   * Returns the application resource file of an application project that will
   * be packaged. The returned {@link File} is not guaranteed to exist.
   */
  public File targetAppFile();

  /**
   * Returns the application upgrade file of an application project that will be
   * packaged.The returned {@link File} is not guaranteed to exist.
   */
  public File targetAppupFile();

  /**
   * Returns the release file of a release project that will be packaged. The
   * returned {@link File} is not guaranteed to exist.
   */
  public File targetRelFile();

  /**
   * Returns the release upgrade file of a release project that will be
   * packaged. The returned {@link File} is not guaranteed to exist.
   */
  public File targetRelupFile();

  /**
   * Returns the system configuration file of a release project that will be
   * packaged. The returned {@link File} is not guaranteed to exist.
   */
  public File targetSysConfigFile();

  /**
   * Returns the directory where profiling reports will be put.
   * 
   * @since 2.1.0
   */
  public File targetProfilingReports();

}
