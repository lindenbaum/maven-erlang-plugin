package eu.lindenbaum.maven;

import java.util.Locale;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.doxia.site.renderer.SiteRenderer;

/**
 * Abstract class to generate the edoc reports.
 */
public abstract class AbstractEdocReportMojo extends AbstractMavenReport {
  /**
   * Doxia Site Renderer.
   * 
   * @parameter expression="${component.org.codehaus.doxia.site.renderer.SiteRenderer}"
   * @required @readonly
   */
  private SiteRenderer siteRenderer;

  /**
   * Project to interact with.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Path to the erlang installation directory.
   * 
   * @parameter
   */
  private String erlPath;

  /**
   * Get the erlPath option.
   * 
   * @return the value of the erlPath option.
   */
  protected final String getErlPath() {
    return this.erlPath;
  }

  /*
   * MavenReport Interface.
   */
  @Override
  protected final MavenProject getProject() {
    return this.project;
  }

  /*
   * MavenReport Interface.
   */
  @Override
  protected final SiteRenderer getSiteRenderer() {
    return this.siteRenderer;
  }

  @Override
  protected final void executeReport(Locale locale) throws MavenReportException {
    if (!canGenerateReport()) {
      return;
    }
    try {
      getLog().debug("Generating " + getDescription(Locale.ENGLISH));

      generateEdocDocumentation();
    }
    catch (MojoExecutionException anException) {
      throw new MavenReportException(anException.getMessage(), anException);
    }
  }

  /**
   * Actually generate the documentation.
   * 
   * @throws MojoExecutionException if a problem occurred with edoc.
   */
  protected abstract void generateEdocDocumentation() throws MojoExecutionException;

  /*
   * MavenReport Interface.
   */
  @Override
  public final boolean isExternalReport() {
    return true;
  }
}
