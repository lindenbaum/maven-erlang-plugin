package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import java.io.File;
import java.util.Locale;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.plugin.AbstractMojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.reporting.MavenReportException;

/**
 * Generates an EDoc report.
 * 
 * @goal edoc
 * @phase generate-sources
 */
public final class EdocReportMojo extends AbstractErlangReport {
  /**
   * Directory where the sources reside.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   * @required
   */
  private File srcMainErlang;

  /**
   * Directory to generate the EDoc report into.
   * 
   * @parameter default-value="${project.reporting.outputDirectory}/doc"
   * @required
   */
  private File docOutput1;

  /**
   * Additional directory to copy the generated EDoc into.
   * 
   * @parameter default-value="${project.build.directory}/doc"
   * @required
   */
  private File docOutput2;

  /**
   * Additional options for EDoc.
   * 
   * @parameter
   */
  private String[] eDocOptions;

  @Override
  protected void executeReport(Locale locale) throws MavenReportException {
    Log log = getLog();
    String description = getDescription(Locale.ENGLISH);
    if (canGenerateReport()) {
      log.debug("Generating " + description);
      this.docOutput1.mkdirs();
      String artifactId = getProject().getArtifactId();
      File appFile = new File(this.srcMainErlang, artifactId + ErlConstants.APP_SUFFIX);
      try {
        if (appFile.exists()) {
          generateAppEDoc(artifactId, this.srcMainErlang, this.docOutput1, this.eDocOptions);
        }
        else {
          generateEDoc(this.srcMainErlang, this.docOutput1, this.eDocOptions);
        }
        copyDirectory(this.docOutput1, this.docOutput2, NULL_FILTER);
      }
      catch (AbstractMojoExecutionException e) {
        throw new MavenReportException(e.getMessage(), e);
      }
    }
    else {
      log.info("No resources to process, skipping " + description);
    }
  }

  @Override
  public boolean canGenerateReport() {
    String[] list = this.srcMainErlang.list();
    return list != null && list.length > 0;
  }

  @Override
  protected String getOutputDirectory() {
    return this.docOutput1.getAbsolutePath();
  }

  @Override
  public String getDescription(Locale locale) {
    return "EDoc Documentation";
  }

  @Override
  public String getName(Locale locale) {
    return "EDoc";
  }

  @Override
  public String getOutputName() {
    return this.docOutput1.getName() + "/index";
  }
}
