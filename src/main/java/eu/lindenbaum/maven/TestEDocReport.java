package eu.lindenbaum.maven;

import java.util.Locale;

import org.apache.maven.plugin.AbstractMojoExecutionException;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.reporting.MavenReportException;

/**
 * <p>
 * This {@link Mojo} will generate EDoc documentation for the test sources found
 * in {@link AbstractErlangReport#srcTestErlang}.
 * </p>
 * <p>
 * The output will be put into {@link AbstractErlangReport#targetSiteTestDoc}.
 * The user may specify custom EDoc options in the project pom using the
 * {@code eDocOptions} parameter.
 * </p>
 * 
 * @goal test-doc
 * @phase generate-test-sources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestEDocReport extends AbstractEDocReport {
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
      this.targetSiteTestDoc.mkdirs();
      try {
        generateEDoc(this.srcTestErlang, this.targetSiteTestDoc, this.eDocOptions);
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
    return this.targetSiteTestDoc.getAbsolutePath();
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
    return this.targetSiteTestDoc.getName() + "/index";
  }
}
