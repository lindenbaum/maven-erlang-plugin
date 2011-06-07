package eu.lindenbaum.maven;

import java.io.File;
import java.util.List;

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
   * Returns the name of the test backend node to use.
   */
  public String testNode();

  /**
   * Returns the cookie that must be used when connecting to the backend node.
   */
  public String cookie();

  /**
   * Returns the {@link SourceLayout} defining this project's source directory
   * structure.
   */
  public SourceLayout sourceLayout();

  /**
   * Returns the {@link TargetLayout} defining this project's build directory
   * structure.
   */
  public TargetLayout targetLayout();

  /**
   * Gathers a complete {@link List} of directories used as code paths in a
   * project's build process.
   */
  public List<File> codePaths();

  /**
   * Gathers a complete {@link List} of directories used as code paths in a
   * project's test build process.
   */
  public List<File> testCodePaths();

  /**
   * Gathers a complete {@link List} of directories used as include directories
   * in a standard build process.
   */
  public List<File> includePaths();

  /**
   * Gathers a complete {@link List} of directories used as include directories
   * in a test build process.
   */
  public List<File> testIncludePaths();

  /**
   * Returns a list of script files used by the plugin to provide a proper test
   * phase, e.g. erlang scripts for surefire reports.
   */
  public List<File> testSupportScripts();

  /**
   * Returns a list of artifacts used by the plugin to provide a proper test
   * phase, e.g. the compiled artifacts of
   * {@link #getTestSupportScripts(Properties)}.
   */
  public List<File> testSupportArtifacts();
}
