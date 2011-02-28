package eu.lindenbaum.maven;

import java.io.File;

import eu.lindenbaum.maven.util.ErlConstants;

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
   * The cookie to use for the java and the backend node.
   * 
   * @parameter expression="${cookie}"
   */
  private String cookie;

  /**
   * The erlang command used to start an erlang backend node. The path must
   * exist and the destination must be executable. If the given command does not
   * fullfill these requirements <code>erl</code> is used (assuming the command
   * is part of the hosts <code>PATH</code>). The path must not contain any
   * arguments.
   * 
   * @parameter expression="${erlCommand}"
   */
  private String erlCommand;

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
    execute(getLog(), getProperties(type, getErlCommand()));
  }

  /**
   * Returns the command to use to start an erlang backend node, also known as
   * the {@code erl} executable.
   * 
   * @return the user configured {@link #erlCommand} or simply {@code erl} if
   *         {@link #erlCommand} was not configured or denotes an invalid path.
   */
  private String getErlCommand() {
    if (this.erlCommand != null) {
      File cmd = new File(this.erlCommand);
      if (cmd.isFile() && cmd.canExecute()) {
        return cmd.getAbsolutePath();
      }
    }
    return ErlConstants.ERL;
  }

  /**
   * Returns properties built from the mojo parameters of this report and based
   * on the packaging type of this project.
   * 
   * @param type the packaging type of the project
   * @param cmd the command used to start a new erlang backend node
   * @return properties for this report
   */
  private Properties getProperties(PackagingType type, String cmd) {
    getLog().debug("Using command: " + cmd);
    return new PropertiesImpl(type, this.project, this.repository, this.base, this.target, cmd, this.cookie);
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
