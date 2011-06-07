package eu.lindenbaum.maven;

import java.io.File;

/**
 * Represents a bean interface holding the directory layout of a project's build
 * directory (build artifacts go here).
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public interface TargetLayout {

  /**
   * Returns the base directory for the build artifacts.
   */
  public File base();

  /**
   * Returns the directory where dependencies get unpacked into.
   */
  public File lib();

  /**
   * Returns the {@link File} representing the main build artifact of the
   * project. This will be used for <code>mvn install/deploy</code>.
   */
  public File projectArtifact();

  /**
   * Returns a {@link File} used by the dialyzer to indicate whether the last
   * dialyzer run was ok (no warnings/errors).
   */
  public File dialyzerOk();

  // applications (erlang-std/erlang-otp) *************************************/

  /**
   * Returns the base directory for the project packaging.
   */
  public File project();

  /**
   * Returns the application resource file of an application project that will
   * be packaged. The returned {@link File} is not guaranteed to exist.
   */
  public File appFile();

  /**
   * Returns the application upgrade file of an application project that will be
   * packaged.The returned {@link File} is not guaranteed to exist.
   */
  public File appupFile();

  /**
   * Returns the directory where the compiled sources will be placed into.
   */
  public File ebin();

  /**
   * Returns the directory where includes to package will be put into.
   */
  public File include();

  /**
   * Returns the directory where private resources will be put into.
   */
  public File priv();

  /**
   * Returns the directory where sources to package will be put into.
   */
  public File src();

  /**
   * Returns the base directory for the tests. This will be accessibly during
   * the test phase by calling <code>code:lib_dir($APPNAME)</code>.
   */
  public File test();

  /**
   * Returns the directory where the compiled test sources and recompiled
   * sources will be placed into.
   */
  public File testEbin();

  /**
   * Returns the directory where the main and test resources will be put into
   * (test resources will override main resources). This will be accessibly
   * during the test phase by calling <code>code:lib_dir($APPNAME, priv)</code>.
   */
  public File testPriv();

  /**
   * Returns the path to the application overview file
   * <code>overview.edoc</code>.
   */
  public File overviewEdoc();

  /**
   * Returns the directory where surefire test reports will be put.
   */
  public File surefireReports();

  /**
   * Returns the directory where profiling reports will be put.
   */
  public File profilingReports();

  // release (erlang-rel) *****************************************************/

  /**
   * Returns the release file of a release project that will be packaged. The
   * returned {@link File} is not guaranteed to exist.
   */
  public File relFile();

  /**
   * Returns the release upgrade file of a release project that will be
   * packaged. The returned {@link File} is not guaranteed to exist.
   */
  public File relupFile();

  /**
   * Returns the system configuration file of a release project that will be
   * packaged. The returned {@link File} is not guaranteed to exist.
   */
  public File sysConfigFile();
}
