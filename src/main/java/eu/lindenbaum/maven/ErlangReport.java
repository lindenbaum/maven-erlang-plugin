package eu.lindenbaum.maven;

import java.io.File;
import java.util.Locale;

import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.doxia.siterenderer.Renderer;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;
import org.apache.maven.reporting.MavenReport;
import org.apache.maven.reporting.MavenReportException;

/**
 * A base class for all {@link MavenReport}s that need to operate on values
 * provided by the {@link PropertiesImpl} bean.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @see PackagingType
 * @see Properties
 */
public abstract class ErlangReport extends AbstractMavenReport {
  /**
   * {@link MavenProject} to process.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Doxia Site Renderer.
   * 
   * @component
   * @required
   * @readonly
   */
  private Renderer renderer;

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
   * @required
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

  @Override
  protected final MavenProject getProject() {
    return this.project;
  }

  @Override
  protected final Renderer getSiteRenderer() {
    return this.renderer;
  }

  /**
   * This may be overwritten by implementing reports, default return value is
   * the absolute path of {@link #getReportOutputDirectory()}.
   */
  @Override
  protected String getOutputDirectory() {
    return this.target.getAbsolutePath();
  }

  /**
   * Injects the needed {@link Properties} into the abstract
   * {@link #execute(Log, Properties)} method to be implemented by subclasses.
   */
  @Override
  protected final void executeReport(Locale locale) throws MavenReportException {
    Properties properties = getProperties();
    try {
      execute(getLog(), locale, properties);
    }
    catch (MojoExecutionException e) {
      throw new MavenReportException(e.getMessage());
    }
    catch (MojoFailureException e) {
      throw new MavenReportException(e.getMessage());
    }
  }

  /**
   * Returns properties built from the mojo parameters of this report and based
   * on the packaging type of this project.
   * 
   * @return properties for this report
   */
  protected Properties getProperties() {
    PackagingType type = PackagingType.fromString(this.project.getPackaging());
    return new PropertiesImpl(type,
                              this.project,
                              this.repository,
                              this.base,
                              this.target,
                              this.node,
                              this.cookie,
                              this.testNode,
                              this.testCookie);
  }

  /**
   * Will be invoked when {@link #execute()} gets invoked on the base class.
   * 
   * @param log logger to be used for output logging
   * @param l the demanded locale as passed to {@link #executeReport(Locale)}
   * @param p to be passed by the base class.
   * @see AbstractMavenReport#executeReport(Locale)
   */
  protected abstract void execute(Log log, Locale l, Properties p) throws MojoExecutionException,
                                                                  MojoFailureException,
                                                                  MavenReportException;
}
