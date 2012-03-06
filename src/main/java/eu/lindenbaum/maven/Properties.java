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
   * 
   * @param withTestCodePaths will also return the code paths needed for test
   *          execution
   */
  public List<File> codePaths(boolean withTestCodePaths);

  /**
   * Gathers a complete {@link List} of directories used as include directories
   * in a standard build process.
   * 
   * @param withTestIncludePaths will also return the include paths needed to
   *          compile test sources
   */
  public List<File> includePaths(boolean withTestIncludePaths);

  /**
   * Returns a list of script files used by the plugin to provide a proper test
   * phase, e.g. erlang scripts for surefire reports.
   */
  public List<File> testSupportScripts();

  /**
   * Returns a list of artifacts used by the plugin to provide a proper test
   * phase, e.g. the compiled artifacts of {@link #testSupportScripts()}.
   */
  public List<File> testSupportArtifacts();

  /**
   * Returns a list of the project's compiled modules.
   * 
   * @param withTests includes test modules into the returned list
   * @param withDependencies includes modules from the project's maven
   *          dependencies (will also include test scope dependencies if
   *          <code>withTestModules</code> is set to {@code true}
   */
  public List<File> modules(boolean withTests, boolean withDependencies);

  /**
   * Returns a list of containing the compiled modules from the project's maven
   * dependencies.
   * 
   * @param withTestScopeDependencies includes modules from the project's maven
   *          test scope dependencies
   */
  public List<File> dependencyModules(boolean withTestScopeDependencies);

  /**
   * Returns a list containing the projects application resource file.
   * optionally the application resource files from the project's maven
   * dependencies are also included.
   * 
   * @param withDependencies includes application resource files from the
   *          project's maven dependencies
   */
  public List<File> applicationFiles(boolean withDependencies);

  /**
   * Returns a list of the project's private resource files.
   * 
   * @param withTests includes test resources into the returned list
   * @param withDependencies includes private resources from the project's maven
   *          dependencies (will also include test scope dependency resources if
   *          <code>withTestModules</code> is set to {@code true}
   */
  public List<File> resources(boolean withTests, boolean withDependencies);
}
