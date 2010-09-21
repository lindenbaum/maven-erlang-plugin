package eu.lindenbaum.maven;

import java.io.File;
import java.util.Locale;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.reporting.MavenReportException;

/**
 * Generate the EDoc report for test sources.
 * 
 * @goal test-edoc
 * @phase generate-test-sources
 */
public final class TestEdocReportMojo extends AbstractErlangReport {
  /**
   * Directory where the test sources reside.
   * 
   * @parameter expression="${basedir}/src/test/erlang/"
   * @required
   */
  private File inputDirectory;

  /**
   * Directory to generate the EDoc report into.
   * 
   * @parameter default-value="${project.reporting.outputDirectory}/test-doc"
   * @required
   */
  private File outputDirectory;

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
      this.outputDirectory.mkdirs();
      try {
        generateEDoc(this.inputDirectory, this.outputDirectory, this.eDocOptions);
      }
      catch (MojoExecutionException e) {
        throw new MavenReportException(e.getMessage(), e);
      }
    }
    else {
      log.info("No resources to process, skipping " + description);
    }
  }

  @Override
  public boolean canGenerateReport() {
    String[] files = this.inputDirectory.list();
    return files != null && files.length > 0;
  }

  @Override
  protected String getOutputDirectory() {
    return this.outputDirectory.getAbsolutePath();
  }

  @Override
  public String getDescription(Locale locale) {
    return "EDoc Documentation For Tests";
  }

  @Override
  public String getName(Locale locale) {
    return "EDoc (Tests)";
  }

  @Override
  public String getOutputName() {
    return this.outputDirectory.getName() + File.separator + "index";
  }
}
