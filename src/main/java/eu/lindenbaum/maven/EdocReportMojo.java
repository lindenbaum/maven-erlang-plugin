package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.EDocUtils.generateAppEDoc;
import static eu.lindenbaum.maven.util.EDocUtils.generateEDoc;

import java.io.File;
import java.util.Locale;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.doxia.siterenderer.Renderer;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;
import org.apache.maven.reporting.MavenReportException;

/**
 * Generate the edoc report.
 * 
 * @goal edoc
 * @phase generate-sources
 */
public final class EdocReportMojo extends AbstractMavenReport {
  /**
   * Project to interact with.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Doxia Site Renderer.
   * 
   * @parameter expression="${component.org.codehaus.doxia.site.renderer.DefaultSiteRenderer}"
   * @required
   * @readonly
   */
  private Renderer siteRenderer;

  /**
   * Directory with module source code.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   */
  private File sourceDirectory;

  /**
   * Destination directory.
   * 
   * @parameter default-value="${project.reporting.outputDirectory}/edoc"
   * @required
   */
  private File outputDirectory;

  /**
   * EDoc options.
   * 
   * @parameter
   */
  private String[] eDocOptions;

  /**
   * @see AbstractMavenReport#getName(Locale)
   */
  @Override
  public String getName(Locale locale) {
    return "EDoc";
  }

  /**
   * @see AbstractMavenReport#getDescription(Locale)
   */
  @Override
  public String getDescription(Locale locale) {
    return "EDoc Documentation";
  }

  /**
   * @see AbstractMavenReport#getProject()
   */
  @Override
  protected MavenProject getProject() {
    return this.project;
  }

  /**
   * @see AbstractMavenReport#getSiteRenderer()
   */
  @Override
  protected Renderer getSiteRenderer() {
    return this.siteRenderer;
  }

  /**
   * @see AbstractMavenReport#isExternalReport()
   */
  @Override
  public boolean isExternalReport() {
    return true;
  }

  /**
   * @see AbstractMavenReport#getOutputDirectory()
   */
  @Override
  protected String getOutputDirectory() {
    return this.outputDirectory.getAbsolutePath();
  }

  /**
   * @see AbstractMavenReport#getOutputName()
   */
  @Override
  public String getOutputName() {
    return this.outputDirectory.getName() + File.separator + "index";
  }

  /**
   * @see AbstractMavenReport#canGenerateReport()
   */
  @Override
  public boolean canGenerateReport() {
    String[] list = this.sourceDirectory.list();
    return list != null && list.length > 0;
  }

  /**
   * @see AbstractMavenReport#executeReport(Locale)
   */
  @Override
  protected void executeReport(Locale locale) throws MavenReportException {
    Log log = getLog();
    String description = getDescription(Locale.ENGLISH);
    if (canGenerateReport()) {
      log.debug("Generating " + description);
      this.outputDirectory.mkdirs();
      String artifactId = getProject().getArtifactId();
      File appFile = new File(this.sourceDirectory, artifactId + ErlConstants.APP_SUFFIX);
      try {
        if (appFile.exists()) {
          generateAppEDoc(getLog(), artifactId, this.sourceDirectory, this.outputDirectory, this.eDocOptions);
        }
        else {
          generateEDoc(getLog(), this.sourceDirectory, this.outputDirectory, this.eDocOptions);
        }
      }
      catch (MojoExecutionException e) {
        throw new MavenReportException(e.getMessage(), e);
      }
    }
    else {
      log.info("No resources to process, skipping " + description);
    }
  }
}
