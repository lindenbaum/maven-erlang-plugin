package eu.lindenbaum.maven;

import java.io.File;
import java.util.Locale;

import org.apache.maven.plugin.AbstractMojoExecutionException;
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
  private File srcTestErlang;

  /**
   * Directory to generate the EDoc report into.
   * 
   * @parameter default-value="${project.reporting.outputDirectory}/test-doc"
   * @required
   */
  private File testDocOutput;

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
      this.testDocOutput.mkdirs();
      try {
        generateEDoc(this.srcTestErlang, this.testDocOutput, this.eDocOptions);
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
    String[] files = this.srcTestErlang.list();
    return files != null && files.length > 0;
  }

  @Override
  protected String getOutputDirectory() {
    return this.testDocOutput.getAbsolutePath();
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
    return this.testDocOutput.getName() + "/index";
  }
}
