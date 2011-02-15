package eu.lindenbaum.maven;

import java.io.File;

import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;

/**
 * A base class for all {@link Mojo}s that need to operate on values provided by
 * the {@link PropertiesImpl} bean.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @see PackagingType
 * @see Properties
 */
public abstract class ErlangMojo extends AbstractMojo {
  /**
   * {@link MavenProject} to process.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * {@link ArtifactRepository} storing dependencies of this
   * {@link MavenProject}.
   * 
   * @parameter expression="${localRepository}"
   * @required
   * @readonly
   */
  private ArtifactRepository repository;

  /**
   * The projects working directory root.
   * 
   * @parameter expression="${basedir}"
   * @required
   * @readonly
   */
  private File base;

  /**
   * The projects build directory.
   * 
   * @parameter expression="${project.build.directory}"
   * @required
   * @readonly
   */
  private File target;

  /**
   * The name of the backend node to use.
   * 
   * @parameter expression="${node}" default-value="maven-erlang-plugin-backend"
   * @required
   */
  private String node;

  /**
   * The cookie to use for the java and the backend node.
   * 
   * @parameter expression="${cookie}" default-value=""
   */
  private String cookie;

  /**
   * The name of the backend node to use.
   * 
   * @parameter expression="${testNode}"
   *            default-value="maven-erlang-plugin-test-backend"
   * @required
   */
  private String testNode;

  /**
   * The cookie to use for the java and the backend node.
   * 
   * @parameter expression="${testCookie}" default-value=""
   */
  private String testCookie;

  /**
   * Injects the needed {@link Properties} into the abstract
   * {@link #execute(Log, Properties)} method to be implemented by subclasses.
   */
  @Override
  public final void execute() throws MojoExecutionException, MojoFailureException {
    PackagingType type = PackagingType.fromString(this.project.getPackaging());
    if (type == PackagingType.UNSUPPORTED) {
      getLog().info("Skipping invocation for packaging type: " + this.project.getPackaging());
      return;
    }
    execute(getLog(), new PropertiesImpl(type,
                                         this.project,
                                         this.repository,
                                         this.base,
                                         this.target,
                                         this.node,
                                         this.cookie,
                                         this.testNode,
                                         this.testCookie));
  }

  /**
   * Will be invoked when {@link #execute()} gets invoked on the base class.
   * 
   * @param log logger to be used for output logging
   * @param p to be passed by the base class.
   * @see AbstractMojo#execute()
   */
  protected abstract void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException;
}
