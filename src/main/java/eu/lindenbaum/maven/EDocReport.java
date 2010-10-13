package eu.lindenbaum.maven;

import java.io.File;
import java.util.Locale;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.plugin.AbstractMojoExecutionException;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.reporting.MavenReportException;

/**
 * <p>
 * This {@link Mojo} will generate EDoc documentation for the sources found in
 * {@link AbstractErlangReport#srcMainErlang}.
 * </p>
 * <p>
 * The output will be put into {@link AbstractErlangReport#targetSite}/doc. The
 * user may specify custom EDoc options in the project pom using the
 * {@code eDocOptions} parameter.
 * </p>
 * <p>
 * BUG It is known that {@code edoc} does not generate a charset meta tag for
 * the resulting {@code html} output.
 * </p>
 * <p>
 * BUG It is known that {@code doxia} overwrites the reports {@code index.html}
 * with a broken template when the report is invoked as standalone.
 * </p>
 * 
 * @goal doc
 * @phase generate-sources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class EDocReport extends AbstractEDocReport {
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
      File outputDir = new File(getReportOutputDirectory(), "doc");
      outputDir.mkdirs();
      String artifactId = getProject().getArtifactId();
      File appFile = new File(this.srcMainErlang, artifactId + ErlConstants.APP_SUFFIX);
      try {
        if (appFile.exists()) {
          generateAppEDoc(artifactId, this.srcMainErlang, outputDir, this.eDocOptions);
        }
        else {
          generateEDoc(this.srcMainErlang, outputDir, this.eDocOptions);
        }
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
    return this.target.getAbsolutePath();
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
    return "doc/index";
  }
}
